/*
 * wallclk = wall clock double
 */

#include <sys/time.h>

double 	wallclk_ ( void )
{
      	struct 	timeval time;
      	double 	tt;

      	gettimeofday(&time,(struct timezone *)0);

      	tt=time.tv_sec+time.tv_usec*1.e-6;

      	return tt;
}
