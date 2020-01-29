C
C -- hardware.h   25-07-95   Version 2.0   Author: Laurent Terray
C    **********   24-12-97   Version 2.2   addition of nsigxxx
C@
C@  Contents : variables related to machine and message passing type
C@  --------
C@
C@ -- cmach : type of machine used to run OASIS
C@
C@ -- cchan : type of message passing used to run OASIS 
C@
C@ -- nsigcld : value of SIGCLD signal (System V)
C@
C@ -- nsigfpe : value of SIGFPE signal (System V)
C@
C@ -- ncatch  : flag value to CATCH signal (System V)
C@
C@ -- nignore : flag value to IGNORE signal (System V)
C@
C@ -- ntiogp : time out in seconds when gathering models (see CLIM manual)
C@
C@ -- ntiret : time in seconds between 2 checks when gathering models (idem)
C@
C@ -- ntiout : time out in seconds WHEN receiving a message (idem)
C@
C     -------------------------------------------------------------------
C
      CHARACTER*4 cmach, cchan
C
      INTEGER nsigcld, nsigfpe, nignore, ncatch
      INTEGER ntiogp, ntiret, ntiout
C
      COMMON / comsgn / nsigcld, nsigfpe, nignore, ncatch
      COMMON / comhdw / cmach, cchan
      COMMON / compvm / ntiret, ntiogp, ntiout
C
C     -------------------------------------------------------------------
