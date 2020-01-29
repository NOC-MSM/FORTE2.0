C
C -- gauss.h   29-09-95   Version 2.0   Author: Laurent Terray
C    *******   25-10-96   Version 2.1   Addition of amskred
C              16-03-99   Version 2.3   Addition of T213 and T319
C              26-03-99   Version 2.3   Changed troncature for number of 
C                                        latitude between equator and pole
C@
C@  Contents : variables related to gaussian troncature (if any spectral model)
C@  --------
C@
C@ -ninip16: number of longitudes for the 16 latitude circles (T21) (1D)
C@
C@ -ninip24: number of longitudes for the 24 latitude circles (T31) (1D)
C@
C@ -ninip32: number of longitudes for the 32 latitude circles (T42) (1D)
C@
C@ -ninip48: number of longitudes for the 48 latitude circles (T63) (1D)
C@
C@ -ninip80: number of longitudes for the 80 latitude circles (T106) (1D)
C@
C@ -ninip160: number of longitudes for the 160 latitude circles (T213-319) (1D)
C@
C@ -- nredu16 : number of points on T21 reduced gaussian grid
C@
C@ -- nredu24 : number of points on T31 reduced gaussian grid
C2
C@ -- nredu32 : number of points on T42 reduced gaussian grid
C@
C@ -- nredu48 : number of points on T63 reduced gaussian grid
C@
C@ -- nredu80 : number of points on T106 reduced gaussian grid
C@
C@ -- nredu160 : number of points on T213-T319 reduced gaussian grids
C@
C@ -- amskred : mask value for reduced grid
C@
C     -------------------------------------------------------------------
C
      INTEGER ninip16, ninip24, ninip32, ninip48, ninip80, 
     $    ninip160, 
     $    nredu16, nredu24, nredu32, nredu48, nredu80,
     $    nredu160
      REAL amskred
C
      COMMON / comred / ninip16(16), ninip24(24), ninip32(32), 
     $                  ninip48(48), ninip80(80), 
     $                  ninip160(160),
     $                  nredu16, nredu24, nredu32, nredu48,
     $                  nredu80, nredu160,
     $                  amskred
C
C     -------------------------------------------------------------------




