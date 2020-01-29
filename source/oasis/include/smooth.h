C
C -- smooth.h   01-10-95   Version 2.0   Author: Laurent Terray
C
C@
C@  Contents : variables controlling how smoothing is handled
C@  --------
C@
C@ Common to all N* variables :
C@       indices for the gcm-1 grid of regions corresponding
C@       to the boundaries of the gcm-2 domain.
C@
C@ -- nsltb : southern outermost point of the smoothing region
C@
C@ -- nslte : southern innermost point of the smoothing region
C@
C@ -- nnltb : northern outermost point of the smoothing region
C@
C@ -- nnlte : northern innermost point of the smoothing region
C@
C@ -- qalfa : control weights in the transition zone (north and south)
C@
C@ -- qbeta : control weights in the transition zone (west and east)
C@
C@ -- nliss : width of the smoothing region (west and east boundaries)
C@
C@ -- nwlgmx : longitude index of the furthest east point on the western  
C@             boundary
C@
C@ -- nelgmx : longitude indexof the furthest west point on the eastern  
C@             boundary
C@
C     -------------------------------------------------------------------
C
      COMMON / comsmo / nsltb, nslte, nnltb, nnlte,
     $                  qalfa, qbeta, nliss, nelgmx, nwlgmx
C
C     -------------------------------------------------------------------
