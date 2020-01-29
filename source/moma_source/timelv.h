c====================== include file "timelv.h" ========================
#ifdef hcomments
c
c     %Z% SCCS module: %M%, version %I%
c
c     time level pointers (indices) 
c
c     baroclinic pointers
c     nm = time level pointer (index) corresponding to "itt-2"
c     nc = time level pointer (index) corresponding to "itt-1"
c     np = time level pointer (index) corresponding to "itt"
c
c     free surface model 
c     nbt = number of free surface model timesteps "dtbt"
c                               per baroclinic timestep "dtuv"
c     nm0 = time level pointer to "itt0-2"
c     nc0 = time level pointer to "itt0-1"
c     np0 = time level pointer to "itt0"
c       where "itt0" is the free surface model timestep.
c
#endif
      integer  nm, nc, np, nbt, nm0, nc0, np0
      common /ctlevp/ nm, nc, np, nbt, nm0, nc0, np0

