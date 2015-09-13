#include<iostream>
#include<fstream>
#include<sstream>
#include<algorithm>
#include<string>
#include<iterator>
#include<vector>
#include<map>

#define TM_FILE "AbileneTM-all/X01"
#define TOPO_FILE "AbileneTM-all/topo-2003-04-10.txt"
#define ROUTING_MATRIX_FILE "AbileneTM-all/A"
#define DEMAND_FILE "AbileneTM-all/demands"
#define FLOW_DIST "./formatted-flows.txt"
#define BASE_PORT 5000
#define TIMEOUT 60



#define THRESH 10

using namespace std;

void read_dmd_indices(unsigned int dmd_index[12][12], map<string, int> routers){
    // read demand indices
    string line;
    ifstream rmf;
    rmf.open(ROUTING_MATRIX_FILE);
    getline(rmf, line);
    getline(rmf, line);
    for(unsigned int i = 0; i<144; i++){
        getline(rmf, line);
        replace(line.begin(), line.end(), ',', ' ');
        stringstream ssline;
        ssline << line;
        string tmp;
        ssline >> tmp; ssline >> tmp; // ignore first pair
        string src, dst;
        ssline >> src; // src-dst
        ssline >> dst;
        unsigned int idx;
        ssline >> tmp;
        ssline >> idx;
        // cout << src << " " << dst << " " << idx << endl;
        dmd_index[routers[src]][routers[dst]] = idx-1;
    }
    rmf.close();
}

void read_dmd_indices_d(unsigned int dmd_index[12][12], map<string, int> routers){
    // read demand indices
    string line;
    ifstream demf;
    demf.open(DEMAND_FILE);
    getline(demf, line);
    for(unsigned int i = 0; i<144; i++){
        getline(demf, line);
        replace(line.begin(), line.end(), ',', ' ');
        stringstream ssline;
        ssline << line;
        string tmp;
        string src, dst;
        ssline >> src; // src-dst
        ssline >> dst;
        unsigned int idx;
        ssline >> idx;
        // cout << src << " " << dst << " " << idx << endl;
        dmd_index[routers[src]][routers[dst]] = idx-1;
    }
    demf.close();
}


void read_flow_byte_sizes(vector<int>& flow_bytes){
    ifstream fs;
    string line;
    fs.open(FLOW_DIST);
    if(!fs.is_open()){
        cout << FLOW_DIST << " could not be opened/found." << endl;
    }
    getline(fs, line);
    while(getline(fs, line)){
        int flow_size;
        int flow_num_pkts;
        stringstream ssline;
        ssline << line;
        string tmp;
        ssline >> tmp; // src ip
        ssline >> tmp; // dst ip
        ssline >> tmp; // prot
        ssline >> tmp; // src port
        ssline >> tmp; // dst port
        ssline >> flow_size; // octets
        ssline >> flow_num_pkts; // pkts
        flow_bytes.push_back(flow_size);
    }
    fs.close();
}


void usage(){
    cout << "Please specify:" << endl;
    cout << "1: node id [1-12]" << endl;
    cout << "2: TM model = 1:realOD, 2: simpleGravityOD, 3:simpleTomogravityOD, 4:generalGravityOD, 5:generalTomogravityOD" << endl;
    cout << "3: Scale 5 mins in traces to x seconds" << endl;
    cout << "4: Dynamic Routing Table? (0/1)" << endl;
    cout << "5: Scale up flow sizes by a factor" << endl;
}

int main(int argc, char *argv[]){
    string line;
    vector<vector<float> > demand; // 100 bytes / 5min
    map<string, int> routers;
    unsigned int dmd_index[12][12];
    vector<int> flow_byte_sizes;
    int node_id;
    int tm_model;
    int dyn_rt = 0;
    float factor = 1;
    // scale down 5 min trace to scaled_to_time seconds
    int scaled_to_time;
    if(argc < 5){
        usage();
        return 1;
    }

    node_id = atoi(argv[1]);
    if(node_id < 1 || node_id > 12){
        cout << "Invalid node_id" << endl;
        return 1;
    }

    tm_model = atoi(argv[2]);
    if(tm_model < 1 || tm_model > 5)
    {
        cout << "Invalid TM model" << endl;
        return 1;
    }
    scaled_to_time = atoi(argv[3]);
    dyn_rt = atoi(argv[4]);
    factor = atof(argv[5]);
    srand(time(NULL));
    read_flow_byte_sizes(flow_byte_sizes);
    /*
       for (std::vector<int>::iterator it = flow_byte_sizes.begin() ; it != flow_byte_sizes.end(); ++it) {
       std::cout << ' ' << *it << endl;
       }
       std::cout << '\n';
       */

    int num_flow_byte_sizes = flow_byte_sizes.size();
    // cout << num_flow_byte_sizes << endl;

    ifstream tm;
    tm.open(TM_FILE);
    for(unsigned int i=0; i<36; i++) {
        vector<float> row;
        for(unsigned int j=0; j<720; j++) {
            float dem;
            tm >> dem;
            if(j%5 == tm_model-1) {
                row.push_back(dem);
            }
        }
        demand.push_back(row);
    }
    tm.close();
    /*
       for(unsigned int i=0; i<2016; i++){
       for(unsigned int j=0; j<144; j++){
       cout << demand[i][j] << " ";
       }
       cout << endl;
       }
       */

    // index the routers
    ifstream topo;
    topo.open(TOPO_FILE);
    int router_id = 0;
    getline(topo, line);
    getline(topo, line);
    for(unsigned int i=0; i<12; i++){
        getline(topo, line);
        stringstream ss;
        ss << line;
        string name;
        ss >> name;
        routers[name] = router_id;
        router_id++;
    }
    topo.close();

    for (std::map<string, int>::iterator it=routers.begin(); it!=routers.end(); ++it){
        std::cout << it->first << " => " << it->second << '\n';
    }

    read_dmd_indices_d(dmd_index, routers);
    /* 
       for(unsigned int src=0; src < 12; src++){
       for(unsigned int dst=0; dst < 12; dst++){
       cout << dmd_index[src][dst] << " ";
       }
       cout << endl;
       }
       */

    // Start writing to replay script
    ofstream replay_script;
    replay_script.open("replay_script.sh");
    replay_script << "#!/bin/sh" << endl << endl;

    replay_script << "pkill -9 iperf" << endl;

    // Schedule clients
    replay_script << endl << "# Start clients" << endl << endl;
    for (std::map<string, int>::iterator src=routers.begin(); src!=routers.end(); ++src){
        // For this source
        if(node_id != src->second+1){
            continue;
        }
        // For each scaled_to_time time
        for (unsigned int time = 0; time < 36; time++){ // TODO: change max time to 2016

            replay_script << "# Time: " << time << endl;
            replay_script << "if [ \"$1\" -eq 1 ] " << endl;
            replay_script << "then" << endl;
            replay_script << "\t cat ../atlas-merlin/10.0.0." << node_id << "_" << time << " > /proc/merlin" << endl;
            replay_script << "fi" << endl;
            replay_script << "./sync-client -s olympic -p 7000 " << endl;

            replay_script << "killall -9 iperf" << endl;
            // iterate over all destinations
            for (std::map<string, int>::iterator dst=routers.begin(); dst!=routers.end(); ++dst){
                // 100 bytes / 5 mins * scaled_to_time s - bytes to be sent in scaled_to_time seconds
                long int rate_bps = demand[time][dmd_index[src->second][dst->second]] * factor *100 / 300 * 8; //bps
                if(rate_bps > 0){
                    replay_script << "iperf -c 10.0.0." << dst->second+1 << " -u -b " << rate_bps << " -t " << scaled_to_time << " &" << endl;
                }
            }
            replay_script << "sleep " << scaled_to_time << endl;
        }
    }

    replay_script << "./sync-client -s olympic -p 7000 "  << endl;
    replay_script << "killall -9 iperf" << endl;
    replay_script.close();

    return 0;
}


