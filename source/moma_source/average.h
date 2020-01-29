c 
c This file contains the common blocks for time-averaging
c
c The data fields:
      _MOMA_REAL 
     &     avt(km,imt,jmt,nt),  !Time average temperature & salinity
     &     avu(km,imt,jmt),     !Time average u velocity (cm/s)
     &     avv(km,imt,jmt),     !Time average v velocity (cm/s)
     &     ref_dens(km),        !Reference densities 

#ifdef tendency_write
     &     tend_adv_av(km,imt,jmt,nt),   !Time average tendency adv
     &     tend_hdiff_av(km,imt,jmt,nt), !Time average tendency hdiff
     &     tend_vdiff_av(km,imt,jmt,nt), !Time average tendency vdiff
#endif
#ifdef nflux_write
     &     avnflux(km,imt,jmt,nt), !Time average northward heat & salt 
#endif

#ifdef rho_is_write
     &     rho_av(km,imt,jmt),  !Time average in situ density (kg/m^3)
#endif
#ifdef ekin_write
     &     ekin(km,imt,jmt),    !Time average kinetic energy (m^2/s^2)
#endif
#ifdef w_write
     &     avw(0:km,imt,jmt),     !Time average w velocity (cm/s)
#endif
#ifdef c_write
     &     avcs(km,imt,jmt),     !Time average c velocity (cm/s)
#endif
#ifdef ice_write
     &     avice(imt,jmt),     !Time average ice coverage (cm/s)
#endif
     &     avu0(imt,jmt),       !Time average barotropic u velocity (cm/s)
     &     avv0(imt,jmt),       !Time average barotropic v velocity (cm/s)
     &     avh0(imt,jmt),       !Time average free surface height (cm)
#ifdef conv_write
     &     avConv(km,imt,jmt),  !Average number of times cell was convectively mixed per time step
#endif
     &     avstf(imt,jmt,nt),    !Average surface temperature (cal/cm^2/sec)/ salinity flux (ppt/cm^2/sec)
     &     avsmf(imt,jmt,nt),    !Average surface  nd stress (dynes/cm**2)
     &     avsnf(imt,jmt,nt)   !Oasis-Average surface temperature (cal/cm^2/sec)/ salinity flux (ppt/cm^2/sec)

      common / av_fields_com / avt,avu,avv,avu0,avv0,avh0,
     &                          avstf,avsmf,avsnf
#ifdef tendency_write
     &     ,tend_adv_av,tend_hdiff_av,tend_vdiff_av
#endif
#ifdef nflux_write
     &     ,avnflux
#endif
#ifdef rho_is_write
     &     ,rho_av
#endif
#ifdef ekin_write
     &     ,ekin
#endif
#ifdef w_write
     &     ,avw
#endif
#ifdef c_write
     &     ,avcs
#endif
#ifdef ice_write
     &     ,avice
#endif
#ifdef conv_write
     &     ,avConv
#endif

      Data ref_dens/24.643600,24.813900,25.071100,25.447900,28.771700,
     & 29.523000,30.725400,32.257900,34.013200,36.065900,38.529100,
     & 41.303700,44.382000,47.693700,51.039900/

c
c  The time accouting information
      _MOMA_REAL 
     &     avtime             !The cumulated averaging time
      common / av_time_com / avtime
