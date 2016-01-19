#pragma once
#include <string>


void predict_part(std::string filename, int totRow, int col, double ** dataM, int period, double scale, double noiselevel,   double demand_jump_factor, double demand_locality_factor, int merge_len);

void mygenerate(int pickwhich,  std::string outputfile, int totRow, double scale, int period, double noiselevel);
void mysynthetic(std::string outputfile, int n_host, int totRow, double scale, std::string datafiledir, int period, double noiselevel, std::string topofile, double demand_jump_factor, double demand_locality_factor, 
        int mer_len);
