C
C -- anais.h   01-11-95   Version 2.0   Author: Laurent Terray
C    *******             
C              31-08-96   Version 2.1 : add new fields for anais package
C@
C@  Contents : variables and arrays related to the ANAIS interpolator
C@  --------
C@
C@ Anaism --->>>
C@
C@ -- nmesh : number of ocean gcm-1 squares overlapped by a given gcm-2 
C@            square (1D)
C@
C@ -- amint : weight for each gcm-1 mesh proportional to overlapped area (1D)
C@
C@ -- nmint : neighbors adress on gcm-1 grid for a given gcm-2 grid point (1D)
C@
C@ -- naismfl : flag to identify different ANAISM parameter sets (1D)
C@
C@ -- naismvoi : maximum number of overlapped neighbors (1D)
C@
C@ -- niwtm : flag to read/write ANAISM parameters (1D)
C@
C@ -- cwanaism : file name for ANAISM parameter file  
C@
C@ Anaisg --->>>
C@
C@ -- agint : weight for each gcm-1 grid point with gaussian distribution (1D)
C@
C@ -- ngint : neighbors adress on gcm-1 grid for a given gcm-2 grid point (1D)
C@
C@ -- varmul : variance multiplicator
C@
C@ -- naisgfl : flag to identify different ANAISG parameter sets (1D)
C@
C@ -- naisgvoi :  maximum number of used neighbors (1D)
C@
C@ -- niwtg : flag to read/write ANAISG parameters (1D)
C@
C@ -- cwanaisg : file name for ANAISG parameter file
C@
C@ Common --->>>
C@
C@ -- linit : I/O initialization flag for each field
C@
C@ -- cnaisout : ANAIS output file name
C@
C     -------------------------------------------------------------------
C
      REAL agint(jpnoa*jpnfg*jpgrd)
      REAL amint(jpwoa*jpnfm*jpgrd), varmul(jpfield)
C
      INTEGER ngint(jpnoa*jpnfg*jpgrd)
      INTEGER nmint(jpwoa*jpnfm*jpgrd)
      INTEGER nmesh(jpnfm*jpgrd), naismfl(jpfield), naisgfl(jpfield)
      INTEGER naismvoi(jpfield), naisgvoi(jpfield)
      INTEGER niwtm(jpfield), niwtg(jpfield)
C
      LOGICAL linit(jpfield)
C
      CHARACTER*8 cwanaisg, cwanaism, cnaisout
C
      COMMON / comesh / agint, amint, ngint, nmint, nmesh,
     $    naismfl, naisgfl, niwtm, niwtg, naismvoi, naisgvoi,
     $    varmul
      COMMON / comlif / linit
      COMMON / comwfl / cwanaisg, cwanaism, cnaisout
C
C     -------------------------------------------------------------------
