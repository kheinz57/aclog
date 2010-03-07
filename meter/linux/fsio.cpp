#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <linux/fsio.h>
#include "aclogfsio.h"

int fd, ints = 0; /* filedescriptor and irq signal handler count */
fsio_irq_t irq;
void (*old_hdl)(int);
void SignalCallBackFunction(unsigned long count);

void irq_sigint_handler(int signum)
{
	static unsigned long irqCount;
	fsio_irq_t irq;
	static bool initOK = false;
	if (ioctl(fd, FSIO_GETIRQ, &irq)) {
		printf("error getting IRQ count\n");
	} else {
		if (!initOK){
			irqCount = irq.count[0];
			initOK = true;
		}

		// check for two more interrupts -> both edges passed
		if ((irq.count[0] - irqCount) > 1) {
			irqCount = irq.count[0];
			SignalCallBackFunction(irqCount);
		}
	}
	ints++;
}

int initPort0Polling()
{
	fd = open("/dev/fsio0", O_RDWR);
	if (fd < 0) {
		printf("Can't open /dev/fsio0\n");
		signal(SIGIO, old_hdl); /* restore */
		return 1;
	}
	return 0;
}

int getPort0Value(unsigned char *data)
{
	int result = read(fd, data, 1);
	if (result != 1) {
	  perror("Can't read byte from port");
	  return 1;
	}
	return 0;
}

int initPort0Irq()
{
	int on = 1, dev;
	unsigned char val;
	sigset_t sigset, old_sigset;

	old_hdl = signal(SIGIO, irq_sigint_handler); /* set new and save old handler */

	fd = open("/dev/fsio0", O_RDWR);
	if (fd < 0) {
		printf("Can't open /dev/fsio0\n");
		signal(SIGIO, old_hdl); /* restore */
		return 1;
	}

	/* descriptor configuration */
	fcntl(fd, F_SETOWN, getpid()); /* set process id that will receive SIGIO */
	ioctl(fd, FIOASYNC, &on); /* I/O by signals */
	ioctl(fd, FIONBIO, &on); /* nonblocking I/O */

	val = 0x01;
	if (ioctl(fd, FSIO_SETMASK, &val)) {
		printf("error on setting iomask\n");
		signal(SIGIO, old_hdl);
		return(1);
	}

	irq.type = 0x00; /* we can only trigger on both edges */
	irq.mask = 0x01; /* use all pins */

	if (ioctl(fd, FSIO_SETIRQ, &irq)) {
		printf("Can't activate IRQs\n");
		signal(SIGIO, old_hdl);
		exit(1);
	}

	sigemptyset(&sigset); /* obtain masked signals */
	sigprocmask(0, NULL, &sigset);
	sigdelset(&sigset, SIGIO); /* unmask SIGIO */
	sigprocmask(SIG_SETMASK, &sigset, &old_sigset); /* apply new mask */
}

int closePort0Irq()
{
	irq.mask = 0x00;
	if (ioctl(fd, FSIO_SETIRQ, &irq)) {
		printf("Warning: Can't deactivate IRQs\n");
	}
	signal(SIGIO, old_hdl); /* restore */
	close(fd);
	return 0;
}

#ifdef STANDALONE
void SignalCallBackFunction(unsigned long count)
{
	printf("count=%lu\n",count);
}

void OutPutTest()
{
    int handle = open("/dev/fsio2", O_RDWR);

	unsigned char b = 0xff; 
	int result = ioctl(handle, FSIO_SETIO, &b);
    if (result) {
          fprintf(stderr, "Warning: Can't set port 3 to output: %s\n", strerror(errno));
    }
	for(int i= 0;i<10;i++){
		b = ~b;
        result = write(handle, &b, 1);
        if (result != 1) {
             perror("Can't write byte to port 0");
             exit(1);
        }
        printf(" 0x%02x\n", b);
        sleep(1);
	}
}

int main (int argc, char* argv[])
{
	if (argc > 1){
		initPort0Irq();
		while(1) sleep(1);
	}
	else
		OutPutTest();

}
#endif
