/*
  Copyright 2010 Heinz Haeberle

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
#include <boost/thread.hpp>
#include <boost/circular_buffer.hpp>
#include "../src/bounded_buffer.hpp"
#include <boost/date_time/posix_time/posix_time.hpp>
#include "boost/format.hpp"
#include "../src/httpget.h"

using namespace std;
using boost::format;

bounded_buffer<boost::posix_time::ptime> msgqueue(50);

void workThread(void)
{
	for(;;){
		boost::posix_time::ptime t;
		msgqueue.pop_back(&t);
		boost::posix_time::ptime time_t_epoch(boost::gregorian::date(1970,1,1));
		boost::posix_time::time_duration diff=t-time_t_epoch;
		std::string server("localhost");
		std::string port("8081");
		std::string s = str(format("hallo %1%") % 10);
		std::string path = str(format("/erl/aclog:logdata?&time=%1%&turns=1") % static_cast<unsigned long long>(diff.total_milliseconds()));
		while(0 != httpgets(server,port,path)){
			boost::this_thread::sleep(boost::posix_time::seconds(2));
		}
	}
}

// a simulation for measuring the time a turn takes.
void waitForNextTurn()
{
	unsigned int t = rand() % 4 + 1;
	boost::this_thread::sleep(boost::posix_time::seconds(t));
}


int main(int argc, char* argv[]) {
	boost::thread worker(workThread);
	srand((int)time(0));
	while(true){
		waitForNextTurn();
		msgqueue.push_front(boost::posix_time::microsec_clock::universal_time());
	}
	return 0;
}

 