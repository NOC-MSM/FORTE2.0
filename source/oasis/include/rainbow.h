C
C -- rainbow.h   05-08-96   Version 2.1   Author: Laurent Terray
C    *********
C@
C@  Contents : variables and arrays related to mapping and subgrid interpolator
C@  --------
C@
C@ Mapping --->>>
C@
C@ -- amapp : weight for each gcm-1 mesh proportional to overlapped area (1D)
C@
C@ -- nmapp : neighbors adress on gcm-1 grid for a given gcm-2 grid point (1D)
C@
C@ -- lmapp : I/O initialization flag for each field
C@
C@ Subgrid --->>>
C@
C@ -- asubg : weight for each gcm-2 mesh proportional to overlapped area (1D)
C@
C@ -- nsubg : neighbors adress on gcm-2 grid for a given gcm-1 grid point (1D)
C@
C@ -- lsubg : I/O initialization flag for each field
C@
C     -------------------------------------------------------------------
C
      REAL amapp(jpmoa*jpnfp*jpgrd), asubg(jpsoa*jpnfs*jpgrd)
C
      INTEGER nmapp(jpmoa*jpnfp*jpgrd), nsubg(jpsoa*jpnfs*jpgrd)
C
      LOGICAL lmapp(jpfield), lsubg(jpfield)
C
      COMMON / commap / amapp, nmapp, asubg, nsubg
      COMMON / comlap / lmapp, lsubg
C
C     -------------------------------------------------------------------
