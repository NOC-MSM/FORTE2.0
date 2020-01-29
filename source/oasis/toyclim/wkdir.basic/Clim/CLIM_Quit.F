      SUBROUTINE CLIM_Quit(kstop,kinfo)
c
c*    *** Quit ***   CLIM 2.0
c
c     purpose:
c     --------
c        leave pvm
c
c     interface:
c     ----------
c        kstop  : flag to exit or not from pvm
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
#include "fpvm3.h"
#include "clim.h"
#include "comclim.h"
c     ----------------------------------------------------------------
      INTEGER   kinfo
c     ----------------------------------------------------------------
      INTEGER*4 info, isize
c     ----------------------------------------------------------------
c
c*    0. First Check
c     --------------
c
      IF ( nexit.ne.1 ) THEN
        kinfo = CLIM_FastExit
        WRITE(nulprt, FMT='(A)') 'Quit - should not be called'
        GO TO 1010
      ENDIF
      kinfo = CLIM_Ok
c
c*    1. proper exit from pvm group
c     -----------------------------
c
      CALL pvmfgsize ( cgroup, isize )
c
      IF ( isize.le.0 ) THEN
        kinfo = CLIM_Group
        WRITE(nulprt, FMT='(A,I3,A)') 
     &        'Quit - pb gsize <pvm ',isize,'>'
      ELSE
        WRITE(nulprt, FMT='(A,I3,A)') 
     &        'Quit - barrier with',isize,' jobs'
        CALL pvmfbarrier ( cgroup, isize, info )
        IF ( info.ne.PvmOk ) THEN
          kinfo = CLIM_Group
          WRITE(nulprt, FMT='(A,I3,A)') 
     &          'Quit - pb barrier <pvm ',info,'>'
        ELSE
          CALL pvmflvgroup ( cgroup, info )
          IF ( info.ne.PvmOk ) THEN
            kinfo = CLIM_Group
            WRITE(nulprt, FMT='(A,I3,A)') 
     &            'Quit - pb lvgroup <pvm ',info,'>'
          ENDIF
        ENDIF
      ENDIF
c
c*    2. conditional exit from pvm
c     ----------------------------
c
      IF ( kstop.eq.CLIM_StopPvm ) THEN
        CALL pvmfexit ( info )
        IF ( info.ne.0 .and. kinfo.eq.CLIM_Ok ) THEN
          kinfo = CLIM_PvmExit
        ENDIF
      ENDIF
c
      WRITE(nulprt, FMT='(A,I3,A)') 
     &      'Quit - exit status <pvm ',info,'>'
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      CALL FISH(nulprt)
      RETURN
      END
