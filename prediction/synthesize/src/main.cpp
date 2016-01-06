#include <iostream>

#include "predict.h"

#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>


using namespace std;
using namespace boost;
using namespace boost::program_options;

int main(int argc, char ** argv)
{

	try {
		std::string appName = boost::filesystem::basename(argv[0]);

		int num_rows = 0;
		int num_hosts = 0;
		int pick_which = 0;
		double scale_factor = 0;
		int period = 0;
		double addn = 0;
		std::string topofile;

		options_description desc("Options");
		desc.add_options()
			("help,h", "Print help messages")
		("name,n", value<std::string>()->required(), "name of the file")
		("pattern,p", value<std::string>()->default_value(""), "location of pattern and pareto")
			("num_rows,r", value<int>(&num_rows)->default_value(20), "consider the first num rows of  the Abilene data.")
			("num_host,m", value<int>(&num_hosts)->default_value(3), "num of hosts")
			("scale_factor,f", value<double>(&scale_factor)->default_value(1.0), "scale_factor")
			("period,d", value<int>(&period)->default_value(3000), "period")
									("topo,t", value<std::string>(&topofile)->default_value(""), "topo file pos")
									("adnoise,a", value<double>(&addn)->default_value(0.5), "add random noise to which level"); 

		positional_options_description positionalOptions;
		positionalOptions.add("num_rows", 1);
		positionalOptions.add("pick_which", 1);
		positionalOptions.add("scale_factor", 1);

		variables_map vm;

		try
		{
			store(command_line_parser(argc, argv).options(desc)
				.positional(positionalOptions).run(), vm);

			if (vm.count("help"))
			{
				cout << desc << "\n";
				return 0;
			}

			notify(vm);

		}
		catch (boost::program_options::required_option& e)
		{
			std::cerr << "ERROR: " << e.what() << std::endl << std::endl;
			return 1;
		}
		catch (boost::program_options::error& e)
		{
			std::cerr << "ERROR: " << e.what() << std::endl << std::endl;
			return 1;
		}

		std::cout << "num_rows = " << num_rows << std::endl;
		std::cout << "pick_which = " << pick_which << std::endl;
		std::cout << "name = " << vm["name"].as<std::string>() << std::endl;
		std::cout << "period = " << vm["period"].as<int>() << std::endl;
		std::cout << "pattern = " << vm["pattern"].as<string>() << std::endl;
		std::cout << "num_host = " << vm["num_host"].as<int>() << std::endl;
		std::cout << "topo = " << vm["topo"].as<string>() << std::endl;

		mysynthetic( vm["name"].as<std::string>(), 
			vm["num_host"].as<int>(),
			num_rows, scale_factor, 
			vm["pattern"].as<std::string>(),
			period, addn,
			vm["topo"].as<std::string>()
			);
		return 0;
	}
	catch (std::exception& e)
	{
		std::cerr << "Unhandled Exception reached the top of main: "
			<< e.what() << ", application will now exit" << std::endl;
		return 1;
	}
  
}
