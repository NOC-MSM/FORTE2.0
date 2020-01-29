C
C -- parallel.h   11-09-95   Version 2.0   Author: Laurent Terray
C    **********
C@
C@  Contents : variables related to parallel data decomposition
C@  --------
C@
C@ -- nparal : parallel decomposition description (see CLIM manual) (2D)
C@
C@ -- cparal : type of parallel decomposition (idem) (1D)
C@
C     -------------------------------------------------------------------
C
      INTEGER nparal(jpparal,jpfield)
C
      CHARACTER*8 cparal(jpfield)
C
      COMMON / comipd / nparal
      COMMON / comcpd / cparal
C
C     -------------------------------------------------------------------




