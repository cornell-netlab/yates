#include <iostream>

#include "predict.h"

#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>


using namespace std;
using namespace boost;
using namespace boost::program_options;

int main(int argc, char ** argv)
{

  try  { 
      std::string appName = boost::filesystem::basename(argv[0]);
      
      int num_rows = 0; 
      int num_hosts = 0;
      double scale_factor = 0;
      
      options_description desc("Options"); 
      desc.add_options() 
	("help,h", "Print help messages") 
	("name,n", value<std::string>()->required(), "name") 
	("num_rows,r", value<int>(&num_rows)->required(), "num rows")
	("num_hosts,h", value<int>(&num_hosts)->required(), "num_hosts") 
	("scale_factor,f", value<double>(&scale_factor)->required(), "scale_factor"); 
 
      positional_options_description positionalOptions; 
      positionalOptions.add("num_rows", 1); 
      positionalOptions.add("num_hosts", 1);
      positionalOptions.add("scale_factor", 1); 
 
      variables_map vm; 
 
     try 
       { 
	 store(command_line_parser(argc, argv).options(desc) 
	       .positional(positionalOptions).run(), vm); 
 
	 if ( vm.count("help")  ) 
	   {
	     cout << desc << "\n";
	     return 0;
	   }
	 
	 notify(vm); 

       } 
     catch(boost::program_options::required_option& e) 
       {
	 std::cerr << "ERROR: " << e.what() << std::endl << std::endl;
	 return 1;
       } 
     catch(boost::program_options::error& e) 
       { 
	 std::cerr << "ERROR: " << e.what() << std::endl << std::endl; 
	 return 1;
       }

     std::cout << "num_rows = " << num_rows << std::endl;    
     std::cout << "name = " << vm["name"].as<std::string>() << std::endl; 
  
  } 
  catch(std::exception& e) 
  { 
    std::cerr << "Unhandled Exception reached the top of main: " 
              << e.what() << ", application will now exit" << std::endl; 
    return 1; 
  } 
 
  //return doit(argc, argv);
  return 0;
  
}
