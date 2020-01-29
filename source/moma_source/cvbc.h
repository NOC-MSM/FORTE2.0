c====================== include file "cvbc.h" ==========================
#ifdef hcomments
c
c     %Z% SCCS module: %M%, version %I%
c
c     vertical boundary condition variables:
c
c     smf = surface momentum flux   (dynes/cm**2)
c          1 => zonal wind stress  2 => meridional wind stress
c     bmf = bottom momentum flux
c          1 => zonal bottom drag  2 => meridional bottom drag 
c     stf = surface tracer flux    
c          1 => surface heat flux (cal/cm**2/sec = cm*degC/sec
c               = ly/sec) (assuming rho*cp = 1 cal/degC/cm**3)
c          2 => surface water flux (ppt/cm**2/sec)
c     btf = bottom tracer flux (for consistency but normally zero!)
c          1 => bottom heat flux   2 => bottom water flux
c
#endif
       _MOMA_REAL smf(2),  bmf(2), stf(nt), btf(nt), snf(nt)
       common /cvbc/ smf,  bmf, stf, btf, snf
#ifdef flux_write
       real surf_t_flux(imt,jmt,nt)
       real surf_n_flux(imt,jmt,nt)
       real surf_m_flux(imt,jmt,nt)
       common /cvbc_surf/ surf_t_flux, surf_n_flux, surf_m_flux
#endif
#ifdef cmip_flxrd
       real surfrd_t_flux(imt,jmt,nt)
       real surfrd_m_flux(imt,jmt,nt)
       common /cvbc_surf/ surfrd_t_flux, surfrd_m_flux
#endif
C$OMP THREADPRIVATE( /cvbc/ )


