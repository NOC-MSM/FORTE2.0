C
C -- coast.h   01-11-95   Version 2.0   Author: Laurent Terray
C    *******
C@
C@  Contents : variables and arrays related to the coast correction
C@  --------
C@
C@ Anaism --->>>
C@
C@    Rappel:
C@ -- nmesh : number of ocean gcm-1 squares overlapped by a given gcm-2 
C@            square (1D)
C@    End of rappel
C@
C@ -- ncoast : number of coastal ocean squares on the gcm-2 grid for which
C@              there are no underlying ocean squares on the gcm-1 grid
C@
C@ -- npcoast : coast mismatch data array (2D)
C@              (n,1) --> 1D index of nth point described above
C@              (n,2) --> number of neighbours suitable for extrapolating SST
C@              (n,3-6) --> 1D indices of neighbours of nth point
C@ 
C@ -- nfcoast : flag to perform coast mismatch correction
C@
C@ -- lcoast  : initialization flag
C@
C     -------------------------------------------------------------------
C
      INTEGER npcoast(jpwoa,6), ncoast, nfcoast
C
      LOGICAL lcoast
C
      COMMON / comcoa / npcoast, ncoast, nfcoast
      COMMON / comloa / lcoast
C
C     -------------------------------------------------------------------
