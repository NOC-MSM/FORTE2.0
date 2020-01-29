C
C -- memory.h   23-08-95   Version 2.0   Author: Laurent Terray
C    ********
C@
C@  Contents : variables related to pseudo-dynamic memory allocation
C@  --------
C@
C@ -- memtot : total memory size of macro integer and real arrays (1D)
C@
C@ -- nused : total memory used in macro integer and real arrays (1D)
C@
C@ -- navail : total memory available in macro integer and real arrays (1D)
C@
C@ -- nsizold : size for each field sub-array (before interpolation) (1D)
C@
C@ -- nsiznew : size for each field sub-array (after interpolation) (1D)
C@
C@ -- nadrold : adress for each field sub-array (before interpolation) (1D)
C@
C@ -- nadrold : adress for each field sub-array (after interpolation) (1D)
C@
C@ -- mskold : macro array for masks (before interpolation) (1D)
C@
C@ -- msknew : macro array for masks (after interpolation) (1D) 
C@
C@ -- nwork : integer work array (1D)
C@
C@ -- fldold : macro array for fields (before interpolation) (1D)
C@
C@ -- fldnew : macro array for fields (after interpolation) (1D)
C@
C@ -- xgrold : macro array for longitudes (before interpolation) (1D)
C@
C@ -- xgrnew : macro array for latitudes (after interpolation) (1D)
C@
C@ -- surold : macro array for mesh surfaces (before interpolation) (1D) 
C@
C@ -- surnew : macro array for mesh surfaces (after interpolation) (1D)
C@
C@ -- work : real work array (1D)
C@
C     -------------------------------------------------------------------   
C
      INTEGER memtot(2), nused(2), navail(2)
      INTEGER nsizold(jpfield), nsiznew(jpfield)
      INTEGER nadrold(jpfield), nadrnew(jpfield)
      INTEGER mskold(jpmxold), msknew(jpmxnew)
      INTEGER  nwork(jpmax)
C
      REAL fldold(jpmxold), fldnew(jpmxnew)
      REAL xgrold(jpmxold), xgrnew(jpmxnew)
      REAL ygrold(jpmxold), ygrnew(jpmxnew)
      REAL surold(jpmxold), surnew(jpmxnew)
      REAL work(jpmax)
C
      COMMON / comemi / mskold, msknew
      COMMON / comemr / fldold, fldnew, xgrold, ygrold, 
     $                  xgrnew, ygrnew, surold, surnew
      COMMON / comptr / nadrold, nadrnew
      COMMON / comsiz / nsizold, nsiznew
      COMMON / commem / memtot, nused, navail
      COMMON / comwrk / work, nwork
C
C     -------------------------------------------------------------------   
