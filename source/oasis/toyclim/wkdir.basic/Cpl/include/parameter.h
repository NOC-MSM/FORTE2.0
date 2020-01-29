C
C -- parameter.h   01-09-95   Version 2.0   Author: Laurent Terray
C    ***********   05-08-96   Version 2.1 
C                             Mods: Add parameters for mapping 
C                                   and subgrid (L. Terray)
C                  20-06-97   Version 2.2 Mods: Add jpbyteint (S. Valcke)
C                  16-12-97                     Add jpext and jpnbn (L. Terray)
C                  31-12-97                     Suppress jpwrk (L. Terray)
C                  12-10-99   Version 2.3 Mods: Add jpnfn (S. Valcke)
C@
C@  Contents : parameter file for OASIS
C@  --------
C@
C@ Integer parameters must begin by jp (doctor norm)
C@
C@ -- jpfield : maximum number of coupling fields
C@
C@ -- jpmodel : maximum number of models
C@
C@ -- jpanal : maximum number of analyses
C@
C@ -- jpcomb : maximum number of fields to be combined in the BLASxxx analyses
C@
C@ -- jpmxold : Memory size of the macro arrays handling fields values and
C@              field grid-related data before interpolation
C@
C@ -- jpmxnew : Memory size of the macro arrays handling fields values and
C@              values and field grid-related data after interpolation
C@
C@ -- jpmax : sup of the last 2
C@
C@ -- jpgrd : maximum grid size 
C@
C@ -- jpred : maximum reduced gaussian grid size
C@
C@ -- jpparal : number of parameters for parallel data decomposition 
C@             (for CLIM, see appendix A of Oasis user's guide)
C@
C@ -- jpwoa : maximum number of underlying neighbors for SURFMESH interpolation
C@
C@ -- jpnoa : number of neighbors for GAUSSIAN interpolation
C@
C@ -- jpmoa : maximum number of underlying neighbors for MOZAIC interpolation
C@
C@ -- jpsoa : maximum number of overlaying neighbors for SUBGRID interpolation
C@
C@ -- jpnfm : maximum number of different SURFMESH interpolations
C@
C@ -- jpnfg : maximum number of different GAUSSIAN interpolations
C@
C@ -- jpnfp : maximum number of different MOZAIC interpolations
C@
C@ -- jpnfs : maximum number of different SUBGRID interpolations
C@
C@ -- jpnfn : maximum number of different NINENN extrapolations
C@
C@ -- jpbyteint : number of bytes per integer 
C@
C@ -- jpbyterea : number of bytes per real 
C@
C@ -- jpbytecha : number of bytes per character 
C@
C@ -- jpext : maximum number of neighbors for extrapolation
C@
C@ -- jpnbn : maximum number of different extrapolation
C@
C     -------------------------------------------------------------------
C
C* Useful numerical values
      PARAMETER (jpeight = 8, jpfour = 4, jpeighty = 80)
C* Essential parameters to dimension the simulation
      PARAMETER (jpfield = 16, jpmodel = 2, jpanal = 12, jpcomb = 2)
C* Parameters to dimension the main big arrays
      PARAMETER (jpmax = 300000, jpmxold = 300000, jpmxnew = 300000)
C* Parameters related to maxima of grid-dimension
      PARAMETER ( jpgrd = 27664, jpred = 3320)
C* Parameters related to type of // data decomposition
      PARAMETER (jpparal = 362)
C* Parameters related to ANAIS(M-G), MOZAIC and SUBGRID interpolation
      PARAMETER (jpwoa = 1, jpnoa = 4, jpmoa = 48, jpsoa = 40)
      PARAMETER (jpnfm = 1, jpnfg = 1, jpnfp = 4, jpnfs = 1, jpnfn=2)
C* Parameters related to SVIPC librairy
      PARAMETER (jpbyteint = 4, jpbytecha = 1,jpbyterea=4)
C* Parameters related to extrapolation method WEIGHT
      PARAMETER (jpext = 1, jpnbn = 1)
C
C     -------------------------------------------------------------------
