c====================== include file "ctmngr.h" ========================
#ifdef hcomments
c
c     %Z% SCCS module: %M%, version %I%
c
c     time manager clock parameters 
c
c     itt     = current time step number
c     itbt    = current barotropic timestep
c     itbtp   = current pass of free surface model timestepping scheme
c     totsec  = accumulated time in seconds from jan 1, year 0
c     totday  = accumulated time in days
c     years   = accumulated time in years
c     stamp   = date & time stamp corresponding to itt
c
#endif
      _MOMA_REAL totsec, totday, years
      integer itt, itbt, itbtp

      character stamp*32
      common /ctmngr/ totsec, totday, years
      common /ctmngi/ itt, itbt, itbtp
      common /ctmngc/ stamp

