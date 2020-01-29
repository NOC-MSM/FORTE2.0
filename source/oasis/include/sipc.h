C
C -- sipc.h   97-08-11   Version 2.2   Author: S.Valcke,A.Piacentini
C    ******
C@
C@  Contents : variables describing pools (set of shared memory segments) 
C@  --------
C@
C@ -- mpoolinit(r/w) : handles associated to model pools for passing initial 
C@                     information(1 for reading and 1 for writing)
C@
C@ -- mpoolidin : handles associated to pools used to pass field to oasis
C@
C@ -- mpoolidou : handles associated to pools used to pass field from oasis
C@ 
C     -------------------------------------------------------------------
C
      INTEGER  mpoolinitr(jpmodel)
      INTEGER  mpoolinitw(jpmodel)
      INTEGER  mpoolidin(jpfield)
      INTEGER  mpoolidou(jpfield)
C
      COMMON / compool / mpoolinitr, mpoolinitw, mpoolidin, mpoolidou
C     -------------------------------------------------------------------
