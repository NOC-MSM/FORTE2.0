C
C -- extrapol.h   16-12-97   Version 2.2   Author: Laurent Terray
C    **********   30-03-99   Version 2.3   READ/WRITE flag, FILE and dataset
C                                           index for NINENN weights
C@
C@  Contents : variables and arrays related to extrapolation
C@  --------
C@
C@ WEIGHT --->>>
C@
C@ -- aextra : weight for each gcm-1 mesh proportional to overlapped area (1D)
C@
C@ -- nextra : neighbors adress on gcm-1 grid for a given gcm-2 grid point (1D)
C@
C@ -- lextra : I/O initialization flag for each field
C@
C@ NINENN --->>>
C@
C@ -- niwtn : flag to read/write EXTRAP/NINENN parameters (1D)
C@
C@ -- niwtng : flag to read/write EXTRAP/NINENN parameters when extrap
C@             is called by GLORED (1D)
C@
C@ -- cwninenn : file name for NINENN parameter FILE
C@
C@ -- nninnfl : flag to identify different EXTRAP/NINENN parameter sets 
C@              within all NINENN/EXTRAP analyses (1D)
C@
C@ -- nninnflg : flag to identify different EXTRAP/NINENN parameter sets 
C@               WHEN extrap is called by GLORED within all NINENN/EXTRAP
C@               analyses (1D)
C@
C@ COMMON --->>>
C@
C@ -- lweight : flag indicating IF EXTRAP/NINENN parameter sets have 
C@              already been calculated or read (.TRUE.) or not (.FALSE.)
C@
C     -------------------------------------------------------------------
C
      REAL aextra(jpext*jpnbn*jpgrd)
C
      INTEGER nextra(jpext*jpnbn*jpgrd)
      INTEGER niwtn(jpfield), nninnfl(jpfield)
      INTEGER niwtng(jpfield), nninnflg(jpfield)
C
      LOGICAL lextra(jpfield), lweight(jpnfn)
C
      CHARACTER*8 cwninenn
C
      COMMON / comext / aextra, nextra
      COMMON / comlew / lextra
      COMMON / comnin / niwtn, nninnfl,niwtng, nninnflg 
      COMMON / comwni / cwninenn
      COMMON / comwei / lweight
C
C     -------------------------------------------------------------------
