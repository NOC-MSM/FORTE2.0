c====================== include file "flux_adj.h" =========================
#ifdef hcomments
c23456789012345678901234567890123456789012345678901234567890123456789012
c=======================================================================
c
c ***************ONLY FOR 4x4deg MODEL*****************************
c   flux adjustments for a 'realistic climate', hopefully - derived	  
c   from ECMWF SST, Levitus SSS and a lot of hope 
c
c======================================================================
#endif
      common /flux/ heat_flux(90,45,12),water_flux(90,45,12)
      real*8 heat_flux, water_flux
      integer ii,jj,ll

