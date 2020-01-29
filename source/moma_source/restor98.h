c====================== include file "restor98.h" =========================
#ifdef hcomments
c23456789012345678901234567890123456789012345678901234567890123456789012
c=======================================================================
c
c ***************ONLY FOR 4x4deg MODEL*****************************
c     Allows surface fields to be restored to values of temperature and
c     salinity taken from levitus98 atlas rather than the zonal mean
c     otherwise in place
c
c======================================================================
#endif
      common /restor/ restortemp(90,45,12),restorsal(90,45,12),
     $                restoruwind(90,45,12),restorvwind(90,45,12)	
      real*8 restortemp, restorsal,restoruwind, restorvwind
      integer ii,jj,ll

