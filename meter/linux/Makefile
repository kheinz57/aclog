CPPARM      = arm-linux-gnueabi-g++
CPPFLAGSARM = -I /opt/boost/include -D PICOCOM1
LDFLAGSARM  = -L /opt/boost/lib/picocom1 -l boost_thread -l boost_system

CPPX86      = g++
CPPFLAGSX86 = -I /opt/boost/include
LDFLAGSX86  = -L /opt/boost/lib/x86 -l boost_thread -l boost_system

#.o.cpp:
#	$(CPP) $(CPPFLAGS) -c $<

all: fsio.arm aclog.arm aclog.x86 

aclog.x86: aclog.cpp ../src/httpget.cpp
	$(CPPX86) $(CPPFLAGSX86) $(LDFLAGSX86) aclog.cpp httpget.cpp -o aclog.x86


aclog.arm: aclog.cpp ../src/httpget.cpp
	$(CPPARM) $(CPPFLAGSARM) $(LDFLAGSARM) fsio.cpp aclog.cpp httpget.cpp -o aclog.arm

fsio.arm: fsio.cpp fsio.cpp
	$(CPPARM) $(CPPFLAGSARM) -D STANDALONE $(LDFLAGSARM) fsio.cpp -o fsio.arm

