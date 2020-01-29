      SUBROUTINE CLIM_Delta(cdnam,ddt,dde,kinfo)
c
c*    *** Delta ***   CLIM 2.0
c
c     purpose:
c     --------
c        returns the difference elapsed time in seconds
c        between local model and remote model
c
c     interface:
c     ----------
c        cdnam   : name of the remote model
c        ddt    : output double real delta time in seconds
c        dde    : output double real max error time in seconds
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
      INTEGER       kinfo
      REAL*8        ddt, dde
c     ----------------------------------------------------------------
      CHARACTER*32  cltest
      INTEGER       ip
c     ----------------------------------------------------------------
c
      kinfo = CLIM_BadName
      ddt = 0.0
      dde = 0.0
c
      WRITE(cltest, FMT='(A32)') cdnam
c
      DO 10 ip=0,CLIM_MaxMod-1
        IF (cnames(ip).eq.cltest) THEN
          ddt = delta(ip)
          dde = delte(ip)
          kinfo = CLIM_Ok
          GO TO 1010
        ENDIF
   10 CONTINUE
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      RETURN
      END
