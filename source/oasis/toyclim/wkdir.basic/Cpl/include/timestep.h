C
C -- timestep.h   29-08-95   Version 2.0beta   Author: Laurent Terray
C    **********   01-02-96   Version 2.0 : addition of nitfn (L. Terray)
C@
C@  Contents : variables controlling timestepping
C@  --------
C@
C@   OASIS information :
C@   -----------------
C@
C@ -- ntime : total simulated time (in seconds)
C@
C@ -- niter : number of iterations
C@
C@ -- nitfn : last iteration number
C@
C@ -- nstep : timestep value (in seconds)
C@
C@   Remote models information :
C@   -------------------------
C@
C@ -- mstep : number of time steps (1D)
C@
C@ -- mfcpl : frequency of coupling in timesteps (1D)
C@
C@ -- mdt   : length of timesteps in seconds (1D)
C@
C     -------------------------------------------------------------------
C
      INTEGER ntime, niter, nitfn, nstep, mstep, mfcpl, mdt
C
      COMMON / comits / ntime, niter, nitfn, nstep
      COMMON / comrts / mstep(jpmodel), mfcpl(jpmodel), mdt(jpmodel)
C
C     -------------------------------------------------------------------




