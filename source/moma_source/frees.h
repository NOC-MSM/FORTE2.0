c====================== include file "frees.h" =========================
#ifdef hcomments
c
c     %Z% SCCS module: %M%, version %I%
c
c     variables for free surface model calculation of external mode
c
c     h0   =  free surface model sea surface height
c     u0   =  free surface model velocity east
c     v0   =  free surface model velocity north
c     h    = depth over "u,v" points
c     hr   = reciprocal depth over "u,v" points
c     zu   = vertically averaged zonal forcing 
c     zv   = vertically averaged meridional forcing 
c     h00 = free surface height at end of baroclinic timesteps nm,nc 
# ifndef free_eb
c     freeav = time average of free surface fields
# endif
c
#endif
      _MOMA_REAL 
     & h0(imt,jmt,3), u0(imt,jmt,3), v0(imt,jmt,3)
     &,h(imt,jmt),    hr(imt,jmt),    zu(imt,jmt),  zv(imt,jmt)
#ifdef NONLIN_FREE_SURFACE
     &,h00(imt,jmt,2)
#endif
#ifndef free_eb
     &,freeav(3,imt,jmt)
#endif
      common /fields/
     & h0, u0, v0
     &,h,    hr,    zu,  zv
#ifdef NONLIN_FREE_SURFACE
     &,h00
#endif
#ifndef free_eb
     &,freeav
#endif
