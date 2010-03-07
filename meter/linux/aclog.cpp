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
// Description : measure the time for one turn of the electric meter
//               and send each turn together with the according time to a webserver
//============================================================================
//#include "stdafx.h"
#include <boost/thread.hpp>
#include <boost/circular_buffer.hpp>
#include "bounded_buffer.hpp"
#include <boost/date_time/posix_time/posix_time.hpp>
#include "boost/format.hpp"
#include "httpget.h"
#ifdef PICOCOM1
#include "aclogfsio.h"
#endif

using namespace std;
using boost::format;

bounded_buffer<boost::posix_time::ptime> msgqueue(5000);
std::string ErlangServer = "aclogserver";

void displayAndChange(pthread_t threadID, bool change)
{
	int retcode;
	int policy;
	struct sched_param param;

    printf("Priority-Range SCHED_FF: %d - %d\n",
            sched_get_priority_min(SCHED_FIFO),
            sched_get_priority_max( SCHED_FIFO ) );

	if ((retcode = pthread_getschedparam(threadID, &policy, &param)) != 0){
		errno = retcode;
		perror("pthread_getschedparam");
		exit(EXIT_FAILURE);
	}

	std::cout << "INHERITED: ";
	std::cout << "policy=" << ((policy == SCHED_FIFO)  ? "SCHED_FIFO" :
							   (policy == SCHED_RR)    ? "SCHED_RR" :
							   (policy == SCHED_OTHER) ? "SCHED_OTHER" :
														 "???")
			  << ", priority=" << param.sched_priority << std::endl;

	if (change){
		policy = SCHED_FIFO;
		param.sched_priority = 4;

		if ((retcode = pthread_setschedparam(threadID, policy, &param)) != 0)
		{
			errno = retcode;
			perror("pthread_setschedparam");
			exit(EXIT_FAILURE);
		}

		std::cout << "  CHANGED: ";
		std::cout << "policy=" << ((policy == SCHED_FIFO)  ? "SCHED_FIFO" :
								   (policy == SCHED_RR)    ? "SCHED_RR" :
								   (policy == SCHED_OTHER) ? "SCHED_OTHER" :
															  "???")
				  << ", priority=" << param.sched_priority << std::endl;
	}
}

void workThread(void)
{
	for(;;){
		boost::posix_time::ptime t;
		msgqueue.pop_back(&t);
		boost::posix_time::ptime time_t_epoch(boost::gregorian::date(1970,1,1));
		boost::posix_time::time_duration diff=t-time_t_epoch;
		std::string port("8081");
		std::string s = str(format("hallo %1%") % 10);
		std::string path = str(format("/erl/aclog:logdata?&time=%1%&turns=1") % static_cast<unsigned long long>(diff.total_milliseconds()));
		while(0 != httpgets(ErlangServer,port,path)){
			boost::this_thread::sleep(boost::posix_time::seconds(2));
		}
	}
}
#ifdef PICOCOM1
void SignalCallBackFunction(unsigned long count)
{
	printf("count=%lu\n",count);
	msgqueue.push_front(boost::posix_time::microsec_clock::universal_time());
}

#ifdef PICOCOM1IRQ
void waitForNextTurn()
{
	initPort0Irq();
	for(;;){
		sleep(1);
	}
}
#else //PICOCOMIRQ
#define ELEMENTS(A) (sizeof(A)/sizeof((A)[0]))
unsigned char edgeDetect(unsigned char newLevel)
{
	static unsigned char lastSamples[3]={0,1,0};
	unsigned char sample;
	unsigned int lastSampleNumber=0;
	static unsigned char lastLevel=0;
	while((lastSamples[0] != newLevel) || (lastSamples[1] != newLevel) || (lastSamples[2] != newLevel)){
		if (getPort0Value(&sample)){
			printf("getPort0Value failed\n");
			exit(1);
		}
//		if (lastSamples[lastSampleNumber] != (0x01 & sample)){
//			printf("sample=%i    {%i,%i,%i},%i\n",sample,lastSamples[0],lastSamples[1],lastSamples[2],lastSampleNumber);
//		}
		lastSamples[lastSampleNumber] = (0x01 & sample);
		lastSampleNumber++; 
		if (lastSampleNumber >= ELEMENTS(lastSamples)){
			lastSampleNumber = 0;
		}
		boost::this_thread::sleep(boost::posix_time::milliseconds(2)); 
	}
	printf("edgeDetect(%i) returned\n",newLevel);
	return newLevel;
}

void waitForNextTurn()
{
	edgeDetect(0x01);
	edgeDetect(0x00);
}
#endif //PICOCOMIRQ 
#else
// a simulation for measuring the time a turn takes.
void waitForNextTurn()
{
	unsigned int t = rand() % 4 + 1;
	boost::this_thread::sleep(boost::posix_time::seconds(t));
}
#endif

int main(int argc, char* argv[]) {
	if (argc > 1){
		ErlangServer = argv[1];
	}
	srand(time(0));
#ifdef PICOCOM1
	boost::thread worker(workThread);
	if (initPort0Polling()){
		printf("can't init port\n"); 
		return 1;
	}
	displayAndChange(pthread_self(),true);
	displayAndChange(worker.native_handle(),false);
#endif
	while(true){
		waitForNextTurn();
		msgqueue.push_front(boost::posix_time::microsec_clock::universal_time());
	}
	return 0;
}

 
