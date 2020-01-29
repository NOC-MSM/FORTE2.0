c====================== include file "pconst.h" =========================
#ifdef hcomments
c
c     %Z% SCCS module: %M%, version %I%
c
c     rules for parameter constants
c
c     use prefix of "c" for whole real numbers (ie: c57 for 57.0)
c     use "m" after prefix for negative values (ie: cm7 for -7.0)
c     use prefix "p" for non repeating fractions (ie: p5 for 0.5)
c     use prefix "r" for reciprocals (ie: r3 for 1/3.0)
c     combine with "e" for scientific notation, 
c       (ie: c5e4 for 5.0e4, c1em10 for 1.0e-10)
c
#endif
      _MOMA_REAL c0,c1,c2,c3,c4,c5,c8,c12,c16,c360,c100
     & , p25,p5,p75,p125
     & ,  epsln
     & ,  c24,c60,c1440
     & ,  r24,r60,r1440,r12,r16,c3r4,secday
      parameter (c0=0.0, c1=1.0, c2=2.0, c3=3.0, c4=4.0, c5=5.0)
      parameter (c8=8.0, c12=12.0, c16=16.0, c360=360.0, c100=100.0)
      parameter (p25=0.25, p5=0.5, p75=0.75, p125=0.125)
      parameter (epsln=1.0e-20)
      parameter (c24=24.0, c60=60.0, c1440=1440.0)
      parameter (r24=c1/c24, r60=c1/c60, r1440=c1/c1440)
      parameter (r12=c1/c12, r16 = c1/c16, c3r4 = c3/c4)
      parameter (secday=c1/(c60*c1440))
#ifdef hcomments
c
c     stdin, stdout, and stderr set the Fortran units for
c     standard input, standard output, and standard error messages
c     (used in read and write statements)
c
#endif
      integer stdin, stdout, stderr
      parameter (stdin = 5, stdout = 6, stderr = 6)
#ifdef hcomments
c
c     define length of words on different machines
c
#endif
# define LOGICAL_N logical*4

