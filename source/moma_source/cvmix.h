c====================== include file "cvmix.h" =========================
#ifdef hcomments
c
c     %Z% SCCS module: %M%, version %I%
c
c     variables used for vertical diffusion
c
c     inputs: (set through namelist)
c
c     fkph   = vertical diffusion coefficient (cm**2/sec)
c     fkpm   = vertical viscosity coefficient (cm**2/sec)
c
c     derived quantities:
c
c     vvc  = vertical viscosity coeff
c     vdc  = vertical diffusion coeff
c
#endif
      _MOMA_REAL fkph, fkpm, vvc(km), vdc(km)
      common /cvmix/  fkph, fkpm
      common /cvmix_com/ vvc, vdc

C$OMP THREADPRIVATE( /cvmix_com/ )

