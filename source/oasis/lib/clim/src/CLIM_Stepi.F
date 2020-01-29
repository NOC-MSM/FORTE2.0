      SUBROUTINE CLIM_Stepi(cdnam,kstep,kfcpl,kdt,kinfo)
c
c*    *** Stepi ***   CLIM 2.0
c
c     purpose:
c     --------
c        retrieve info on cdnam models time steps
c
c     interface:
c     ----------
c        cdnam   : character string name of the model
c        kstep  : number of step in the simulation
c        kfcpl  : frequency of coupling in steps
c        kdt    : length of a time step in seconds
c        kinfo  : output status
c
c     lib mp:
c     -------
c        pvm3.3
c
c     author:
c     -------
c        Eric Sevault   - METEO FRANCE
c        Laurent Terray - CERFACS
c
c
c     ----------------------------------------------------------------
#include "clim.h"
#include "comclim.h"
c     ----------------------------------------------------------------
      CHARACTER*(*) cdnam
      INTEGER       kstep, kdt, kfcpl, kinfo
c     ----------------------------------------------------------------
      CHARACTER*32  cltest
      INTEGER       ip
c     ----------------------------------------------------------------
c
c*    0. First Check
c     --------------
c
      IF ( nexit.ne.1 ) THEN
        kinfo = CLIM_FastExit
        WRITE(nulprt, FMT='(A)') 'Stepi - should not be called'
        GO TO 1010
      ENDIF
      kinfo = CLIM_Ok
c
c*    1. check for the cdnam model in my list
c     ---------------------------------------
c
      kinfo = CLIM_BadName
      kstep = CLIM_Void
      kdt   = CLIM_Void
      kfcpl = CLIM_Void
c
      WRITE(cltest, FMT='(A32)') cdnam
c
      DO 100 ip=0,CLIM_MaxMod-1
        IF ( cnames(ip).eq.cltest ) THEN
          kstep = nstep(ip)
          kdt   = nstdt(ip)
          kfcpl = nfcpl(ip)
          kinfo = CLIM_Ok
          GO TO 1010
        ENDIF
  100 CONTINUE
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      CALL FISH(nulprt)
      RETURN
      END
