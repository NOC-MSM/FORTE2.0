c====================== include file "param.h" =========================
#ifdef hcomments
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c     %Z% SCCS module: %M%, version %I%
c
c     main parameter file which sets ocean characteristics:
c
c     imt    = number of grid points in longitudinal direction
c     jmt    = number of grid points in latitudinal direction
c     km     = number of grid points in the vertical direction
c     nt     = number of tracers
c     ndyn   = number of dynamically active tracers: =1 just T, = 2 w salinity
c     nhalo  = number of halo rows (1 for five point horizontal
c              schemes, 2 for seven point schemes like MSQ)
c
c   imt and jmt include the extra halo rows
c
#endif
#ifdef msq
# ifndef halo2
# define halo2
# endif
#endif

      integer imt,jmt,km,nt,imu,nhalo,ndyn
#ifdef halo2
      parameter  (nhalo=2)
#else
      parameter  (nhalo=1)
#endif
c      parameter  (imt=90+2*nhalo, jmt=43+2*nhalo, km=15, nt=2, imu=imt)
c      parameter  (imt=30, jmt=30, km=15, nt=2, imu=imt)
c jods:
c      parameter  (imt=28+2*nhalo, jmt=28+2*nhalo, km=15, nt=2, imu=imt)
      parameter  (imt=180+2*nhalo, jmt=86+2*nhalo, km=15, nt=2, imu=imt)

      parameter(ndyn = 2)
      integer imtp1,imtm1,imtm2,imum1,imum2,jmtp1,jmtm1,jmtm2
      integer kmp1,kmp2,kmm1
      integer icbeg,icend,imtm2h
      integer jcbeg,jcend,imtm1h
      parameter  (imtp1=imt+1, imtm1=imt-1, imtm2=imt-2, 
     &            imum1=imu-1, imum2=imu-2, jmtp1=jmt+1, 
     &            jmtm1=jmt-1, jmtm2=jmt-2, 
     &            kmp1=km+1, kmp2=km+2, kmm1=km-1,
     &            icbeg=nhalo+1, icend=imt-nhalo, imtm2h=imt-2*nhalo,
     &            jcbeg=nhalo+1, jcend=jmt-nhalo, imtm1h=imt-nhalo)
#ifdef hcomments
c
c     add parameter constants
c
#endif
#include "pconst.h"

