/*
 * wallclk = wall clock double
 */

#include <sys/time.h>

#if defined(SUN4) || defined(SUN4SOL2) || defined(VPP300) || defined(SGIMP64)
double 	wallclk_ ( void )
#else
#if defined(CRAY)
double	WALLCLK ( void )
#else
double 	wallclk_ ( void )
#endif
#endif
{
      	struct 	timeval time;
      	double 	tt;

      	gettimeofday(&time,(struct timezone *)0);

      	tt=time.tv_sec+time.tv_usec*1.e-6;

      	return tt;
}
