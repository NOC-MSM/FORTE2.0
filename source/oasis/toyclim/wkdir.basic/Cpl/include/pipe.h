C
C -- pipe.h   25-09-95   Version 2.0   Author: Laurent Terray
C    ******   20-09-97   Version 2.2   Mods: suppress nproc (S. Valcke)
C@
C@  Contents : variables describing pipes (FIFO) used for message passing
C@  --------
C@
C@ -- cprnam : files associated to reading model pipes (1D)
C@
C@ -- cpwnam : files associated to writing model pipes (1D)
C@
C     -------------------------------------------------------------------
C
      CHARACTER*8 cprnam(jpmodel), cpwnam(jpmodel)
C
      COMMON / compip / cprnam, cpwnam
C
C     -------------------------------------------------------------------
