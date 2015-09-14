#include<iostream>
#include<fstream>
#include<sstream>
#include<algorithm>
#include<string>
#include<iterator>
#include<vector>
#include<map>
#include<math.h>

#define TM_FILE "AbileneTM-all/X01"
#define TOPO_FILE "AbileneTM-all/topo-2003-04-10.txt"
#define ROUTING_MATRIX_FILE "AbileneTM-all/A"
#define DEMAND_FILE "AbileneTM-all/demands"
#define FLOW_DIST "./formatted-flows.txt"
#define BASE_PORT 5000
#define TIMEOUT 60



#define THRESH 10

using namespace std;

/* return time till next flow assuming Poisson process */
float poissonInterval(int num_flows){
    return -logf(1.0f - (float) random() / (RAND_MAX)) / num_flows;
}

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
    float intervalSleepTime = 0;

    // Parse arguments
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

    int num_flow_byte_sizes = flow_byte_sizes.size();

    // Read TM
    ifstream tm;
    tm.open(TM_FILE);
    for(unsigned int i=0; i<30; i++) {
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

    // index the routers; read topology
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

    // Start writing to replay script
    // Prologue
    ofstream replay_script;
    replay_script.open("replay_script.sh");
    replay_script << "#!/bin/sh" << endl << endl;

    replay_script << "pkill -9 client" << endl;
    replay_script << "pkill -9 server" << endl;

    // Start servers
    replay_script << "# Start servers" << endl;
    for (std::map<string, int>::iterator rcv_from=routers.begin(); rcv_from!=routers.end(); ++rcv_from){
        replay_script << "./tcp/server -p " << (BASE_PORT + rcv_from->second + 1) << " > ./src-" << (rcv_from->second+1) << "-dst-" << node_id << ".txt &" << endl;
    }

    replay_script << "sleep 5";

    // Schedule clients
    replay_script << endl << "# Start clients" << endl << endl;
    for (std::map<string, int>::iterator src=routers.begin(); src!=routers.end(); ++src){
        // For this source
        if(node_id != src->second+1){
            continue;
        }
        // For each scaled_to_time time
        for (unsigned int time = 0; time < 10; time++){ // TODO: change max time to 2016
            long cumul = 0;
            int num_flows = 0;
            // store flow sizes per destination
            map<string, vector<long> > send_size;

            // iterate over all destinations
            for (std::map<string, int>::iterator dst=routers.begin(); dst!=routers.end(); ++dst){
                // 100 bytes / 5 mins * scaled_to_time s - bytes to be sent in scaled_to_time seconds
                float dmd = demand[time][dmd_index[src->second][dst->second]]*100 / 300 * scaled_to_time;

                long cumulsd = 0;

                // flow sizes for this destination
                vector<long> send_size_perdest;
                while(dmd > THRESH){
                    long rand_flow_size = flow_byte_sizes[rand() % num_flow_byte_sizes]*100 * 1; //TODO: Note: multiplying each flow size
                    rand_flow_size = rand_flow_size < dmd ? rand_flow_size : dmd;
                    send_size_perdest.push_back(rand_flow_size);
                    dmd -= rand_flow_size;
                    cumulsd += rand_flow_size;
                    num_flows++;
                }
                cumul += cumulsd;
                send_size[dst->first] = send_size_perdest;
            }
            cout << cumul << endl;
            replay_script << "# Time slept: " << intervalSleepTime << endl;
            intervalSleepTime = 0;
            replay_script << "# Time: " << time << endl;

            // Change routes if dynamic scheme
            replay_script << "if [ \"$1\" -eq 1 ] " << endl;
            replay_script << "then" << endl;
            replay_script << "\t cat ../atlas-kulfi/10.0.0." << node_id << "_" << time << " > /proc/kulfi" << endl;
            replay_script << "fi" << endl;

            // Sync with other servers and kill existing flows
            replay_script << "./sync-client -s olympic -p 7000 " << endl;
            replay_script << "killall -2 client" << endl;

            // Schedule flows
            for(int t=0; t<10; t++){
                float sleepTime = 0;
                for (std::map<string, int>::iterator dst=routers.begin(); dst!=routers.end(); ++dst){
                    for(int i=t; i<send_size[dst->first].size(); i+=10){
                        replay_script << "\t ./tcp/client -s 10.0.0." << dst->second+1 << " -p " << (BASE_PORT + node_id) << " -l " << (int)(send_size[dst->first][i] * factor) << " >> ./flow-time-src-" << node_id << "-dst-" << dst->second+1 << ".txt &" << endl;
                        sleepTime = scaled_to_time * poissonInterval(num_flows);
                        replay_script << "\t sleep " << sleepTime << endl;
                        intervalSleepTime += sleepTime;
                    }
                }
            }
        }
    }

    // Epilogue
    replay_script << "./sync-client -s olympic -p 7000 "  << endl;
    // replay_script << "pkill -9 client" << endl;
    replay_script << "FAIL=0" << endl;
    replay_script << "for job in `jobs -l | grep ./tcp/client | awk '{print $2}'`" << endl;
    replay_script << "do" << endl;
    replay_script << "    echo $job" << endl;
    replay_script << "    wait $job || let \"FAIL+=1\"" << endl;
    replay_script << "done" << endl;
    replay_script << "sleep " << 5 << endl;
    replay_script << "pkill -9 server" << endl;
    replay_script.close();

    return 0;
}


