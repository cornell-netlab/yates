#include<iostream>
#include<fstream>
#include<sstream>
#include<algorithm>
#include<string>
#include<iterator>
#include<map>
#include<math.h>
#include<sys/time.h>
#include<unistd.h>
#include<vector>

#define TM_FILE "AbileneTM-all/X01"
#define TOPO_FILE "AbileneTM-all/topo-2003-04-10.txt"
#define ROUTING_MATRIX_FILE "AbileneTM-all/A"
#define DEMAND_FILE "AbileneTM-all/demands"
#define FLOW_DIST "./formatted-flows.txt"
#define BASE_PORT 5000
#define FLOW_SIZE_SCALE_UP 80
#define THRESH 10

#define TM_NUM_ROWS 36
#define DRY_RUN 0


using namespace std;

/* return time till next flow assuming Poisson process */
float poisson_interval(int total_num_flows, float time_interval)
{
    return -logf(1.0f - (float)random() / (RAND_MAX)) / total_num_flows * time_interval;
}

/* return t2 - t1 time in sec */
float diff_time(struct timeval t2, struct timeval t1){
 return (t2.tv_sec - t1.tv_sec) + (float)(t2.tv_usec - t1.tv_usec)/1000000;
}

/* execute the given command */
int execute(const char *cmd)
{
    int ret = 0;
    if (DRY_RUN) {
        ofstream cmd_log;
        cmd_log.open("cmd_log.sh", ofstream::out | ofstream::app);
        cmd_log << cmd << endl;
        cmd_log.close();
    } else {
       ret = system(cmd);
    }
    return ret;
}

void read_dmd_indices(unsigned int dmd_index[12][12], map < string, int >routers)
{
    // read demand indices
    string line;
    ifstream rmf;
    rmf.open(ROUTING_MATRIX_FILE);
    getline(rmf, line);
    getline(rmf, line);
    for (unsigned int i = 0; i < 144; i++) {
        getline(rmf, line);
        replace(line.begin(), line.end(), ',', ' ');
        stringstream ssline;
        ssline << line;
        string tmp;
        ssline >> tmp;
        ssline >> tmp;	// ignore first pair
        string src, dst;
        ssline >> src;	// src-dst
        ssline >> dst;
        unsigned int idx;
        ssline >> tmp;
        ssline >> idx;
        // cout << src << " " << dst << " " << idx << endl;
        dmd_index[routers[src]][routers[dst]] = idx - 1;
    }
    rmf.close();
}

void
read_dmd_indices_d(unsigned int dmd_index[12][12], map < string, int >routers)
{
    // read demand indices
    string line;
    ifstream demf;
    demf.open(DEMAND_FILE);
    getline(demf, line);
    for (unsigned int i = 0; i < 144; i++) {
        getline(demf, line);
        replace(line.begin(), line.end(), ',', ' ');
        stringstream ssline;
        ssline << line;
        string tmp;
        string src, dst;
        ssline >> src;	// src-dst
        ssline >> dst;
        unsigned int idx;
        ssline >> idx;
        // cout << src << " " << dst << " " << idx << endl;
        dmd_index[routers[src]][routers[dst]] = idx - 1;
    }
    demf.close();
}

int read_flow_byte_sizes(vector < int >&flow_bytes)
{
    ifstream fs;
    string line;
    fs.open(FLOW_DIST);
    float total_flow_size = 0;
    if (!fs.is_open()) {
        cout << FLOW_DIST << " could not be opened/found." << endl;
    }
    getline(fs, line);
    while (getline(fs, line)) {
        int flow_size;
        int flow_num_pkts;
        stringstream ssline;
        ssline << line;
        string tmp;
        ssline >> tmp;	// src ip
        ssline >> tmp;	// dst ip
        ssline >> tmp;	// prot
        ssline >> tmp;	// src port
        ssline >> tmp;	// dst port
        ssline >> flow_size;	// octets
        ssline >> flow_num_pkts;	// pkts
        flow_size *= FLOW_SIZE_SCALE_UP;
        flow_bytes.push_back(flow_size);
        total_flow_size += flow_size;
    }
    fs.close();
    return (int)(total_flow_size/flow_bytes.size());
}

int sample_flow_size(vector < int >fs_dist, int fs_len)
{
    return fs_dist[rand() % fs_len];
}

void usage()
{
    cout << "Please specify:" << endl;
    cout << "1: node id [1-12]" << endl;
    cout <<
        "2: TM model = 1:realOD, 2: simpleGravityOD, 3:simpleTomogravityOD, 4:generalGravityOD, 5:generalTomogravityOD"
        << endl;
    cout << "3: Scale 5 mins in traces to x seconds" << endl;
    cout << "4: Dynamic Routing Table? (0/1)" << endl;
    cout << "5: Scale up flow sizes by a factor" << endl;
    cout << "6: Run id" << endl;
}

int main(int argc, char *argv[])
{
    string line;
    vector <vector <float> > D;	// 100 bytes / 5min
    map < string, int > routers;
    unsigned int dmd_index[12][12];

    vector <int> flow_byte_sizes;   // Flow size distribution
    int num_flow_byte_sizes;

    int node_id;        // Node ID [1,12]
    int tm_model;       // TM model
    int dyn_rt = 0;     // Should routes be dynamically updated
    float factor = 1;   // scaling factor for demands
    int scaled_to_time; // scale down 5 min trace to scaled_to_time seconds
    int run_id; 	// Run id
    if (argc < 7) {
        usage();
        return 1;
    }

    float interval_sleep_time = 0, pre_process_time = 0;
    struct timeval now, last_update, pre_process_complete;
    int interval = 0;	// TM series time
    long total_demand_scheduled = 0;
    int total_num_flows = 0;
    map < string, vector < long > > send_size;
    int send_size_now;
    int num_flows_sent = 0;
    int avg_flow_size = 0;
    float actual_dst_sent[12];
    float actual_total_sent;
    float dst_remaining, total_remaining;
    stringstream cmd;   // system command
    /* Parse arguments */
    node_id = atoi(argv[1]);
    if (node_id < 1 || node_id > 12) {
        cout << "Invalid node_id" << endl;
        return 1;
    }

    tm_model = atoi(argv[2]);
    if (tm_model < 1 || tm_model > 5) {
        cout << "Invalid TM model" << endl;
        return 1;
    }
    scaled_to_time = atoi(argv[3]);
    dyn_rt = atoi(argv[4]);
    factor = atof(argv[5]);
    run_id = atoi(argv[6]);

    srand(run_id);

    /* Read flow size distribution */
    avg_flow_size = read_flow_byte_sizes(flow_byte_sizes) * factor;
    num_flow_byte_sizes = flow_byte_sizes.size();
    cout << "Avg flow size: " << avg_flow_size << endl;
    /* Read Abilene TM */
    ifstream tm;
    tm.open(TM_FILE);
    for (unsigned int i = 0; i < TM_NUM_ROWS; i++) {
        vector < float >row;
        for (unsigned int j = 0; j < 720; j++) {
            float dem;
            tm >> dem;
            if (j % 5 == tm_model - 1) {
                // Abilene data units are in 100B/5min
                row.push_back(dem * 100/300 * scaled_to_time * factor);
            }
        }
        D.push_back(row);
    }
    tm.close();

    /* index the routers; read topology */
    ifstream topo;
    topo.open(TOPO_FILE);
    int router_id = 0;
    getline(topo, line);
    getline(topo, line);
    for (unsigned int i = 0; i < 12; i++) {
        getline(topo, line);
        stringstream ss;
        ss << line;
        string name;
        ss >> name;
        routers[name] = router_id;
        router_id++;
    }
    topo.close();

    for (std::map < string, int >::iterator it = routers.begin();
            it != routers.end(); ++it) {
        std::cout << it->first << " => " << it->second << '\n';
    }

    read_dmd_indices_d(dmd_index, routers);

    /* Start replaying */
    cmd << "pkill -9 client" << endl;;
    execute(cmd.str().c_str());
    cmd.str("");
    cmd << "pkill -9 server" << endl;
    execute(cmd.str().c_str());
    cmd.str("");

    /* Start servers */
    cmd << " echo \"# Start servers\"" << endl;
    execute(cmd.str().c_str());
    cmd.str("");
    for (std::map < string, int >::iterator rcv_from = routers.begin();
            rcv_from != routers.end(); ++rcv_from) {
        cmd << "./tcp/server -p "
            << (BASE_PORT + rcv_from->second + 1)
            << " > ./src-" << (rcv_from->second + 1)
            << "-dst-" << node_id << ".txt &";
        execute(cmd.str().c_str());
        cmd.str("");
    }

    /* Start pings */
    cmd << " echo \"# Start pings\"";
    execute(cmd.str().c_str());
    cmd.str("");
    for (std::map < string, int >::iterator rcv_from = routers.begin();
            rcv_from != routers.end(); ++rcv_from) {
        cmd << "ping 10.0.0." << (rcv_from->second +1)
            << " > ./ping-" << node_id << "-"
            << (rcv_from->second + 1) << ".txt &";
        execute(cmd.str().c_str());
        cmd.str("");
    }

    cmd << "sleep 1";
    execute(cmd.str().c_str());
    cmd.str("");

    int src_id = node_id - 1;

    cmd << "echo \"# Start clients\"";
    execute(cmd.str().c_str());
    cmd.str("");
    last_update.tv_sec = 0;

    /* Sychronize with other nodes */
    cmd << "./sync-client -s olympic -p 7000 " << endl;
    execute(cmd.str().c_str());
    cmd.str("");
    /* Loop for all rows in TM */
    while (1) {
        gettimeofday(&now, NULL);
        float rand_throw = (float)rand()/RAND_MAX;
        float cumul_throw = 0;
        if (diff_time(now, last_update) >= scaled_to_time) {
            /* New TM interval */
            if(interval>0){
		    cmd << "echo \"# Time slept: " << interval_sleep_time << "\"";
        	    execute(cmd.str().c_str());
	            cmd.str("");
		    for (std::map < string, int >::iterator dst =
				    routers.begin(); dst != routers.end(); ++dst) {
                       cout << dst->second << "\t" << actual_dst_sent[dst->second] << " / " << D[interval-1][dmd_index[src_id][dst->second]] << " (" << actual_dst_sent[dst->second]/D[interval-1][dmd_index[src_id][dst->second]] * 100.0 << " %)" << endl ;
		    }
		    cout << num_flows_sent << " / " << total_num_flows << endl; 
	    } 
            interval_sleep_time = 0;
            cmd << "echo \"# Time: " << interval << "\"";
            execute(cmd.str().c_str());
            cmd.str("");
            if(interval >= TM_NUM_ROWS){
                break;
            }
            last_update = now;
            total_demand_scheduled = 0;
            total_num_flows = 0;
            num_flows_sent = 0;

            // store flow sizes per destination
            for (std::map < string, int >::iterator dst =
                    routers.begin(); dst != routers.end(); ++dst) {
                total_demand_scheduled += D[interval][dmd_index[src_id][dst->second]];
                actual_dst_sent[dst->second] = 0;
            }
            actual_total_sent = 0;
            total_num_flows = (int)(total_demand_scheduled/avg_flow_size);
            /* Change routes if dynamic scheme */
            if (dyn_rt) {
                cmd << "cat ./routes/10.0.0." << node_id << "_"
                    << interval << " > /proc/kulfi";
                execute(cmd.str().c_str());
                cmd.str("");
            }
            interval++;
            gettimeofday(&pre_process_complete, NULL);
            pre_process_time = diff_time(pre_process_complete, now);
            cout << pre_process_time << endl;
        }

        /* Select a dst (weighted by demmand) and start a flow */
        for (std::map < string, int >::iterator dst =
                routers.begin(); dst != routers.end(); ++dst) {
                dst_remaining = D[interval-1][dmd_index[src_id][dst->second]] - actual_dst_sent[dst->second];
                dst_remaining = dst_remaining > 0 ? dst_remaining : 1;
                total_remaining = (total_demand_scheduled > actual_total_sent) ? (total_demand_scheduled - actual_total_sent) : 12;
		cumul_throw += dst_remaining/total_remaining;
		if(cumul_throw > rand_throw){ 
		    send_size_now = sample_flow_size(flow_byte_sizes, num_flow_byte_sizes) * factor;
		    if (dst_remaining != 1){
                        send_size_now = (send_size_now < (1.1 * dst_remaining)) ? send_size_now : (1.1 * dst_remaining);
		    }
		    cmd << "./tcp/client -s 10.0.0."
			    << (dst->second + 1) << " -p "
			    << (BASE_PORT + node_id)
			    << " -l " << send_size_now << " -n " << 1
			    << " >> ./flow-time-src-" << node_id << "-dst-"
			    << (dst->second + 1) << ".txt &";
		    execute(cmd.str().c_str());
		    cmd.str("");
		    num_flows_sent++;
                    actual_dst_sent[dst->second] += send_size_now;
                    actual_total_sent += send_size_now;
		    float sleep_time = poisson_interval(total_num_flows, 0.8 * (scaled_to_time-pre_process_time));
		    cmd << "# sleep " << sleep_time;
		    execute(cmd.str().c_str());
		    usleep((unsigned int)(sleep_time * 1000000));
		    cmd.str("");
		    interval_sleep_time += sleep_time;
		    break;
	    }
        }
    }

    /* Clean up */
    cmd << "./sync-client -s olympic -p 7000 " << endl;
    execute(cmd.str().c_str());
    cmd.str("");
    cmd << "sleep " << 2 << endl;
    execute(cmd.str().c_str());
    cmd.str("");
    cmd << "pkill -9 server" << endl;
    execute(cmd.str().c_str());
    cmd.str("");
    cmd << "pkill -9 ping" << endl;
    execute(cmd.str().c_str());
    cmd.str("");

    return 0;
}
