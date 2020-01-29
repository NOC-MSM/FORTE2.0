      SUBROUTINE CLIM_Reset(kinfo)
c
c*    *** Reset ***   CLIM 2.0
c
c     purpose:
c     --------
c        Reset the CLIM experiment
c
c     interface:
c     ----------
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
      INTEGER       kinfo
c     ----------------------------------------------------------------
      INTEGER*4     isize, info
c     ----------------------------------------------------------------
c
c*    1. proper exit from pvm group
c     -----------------------------
c
      kinfo  = CLIM_Ok
c
      CALL pvmfgsize ( cgroup, isize )
c
      IF ( isize.le.0 ) THEN
        kinfo = CLIM_Group
        WRITE(nulprt, FMT='(A,I3,A)') 
     &        'Reset - pb gsize <pvm ',isize,'>'
      ELSE
        WRITE(nulprt, FMT='(A,I2,A)') 'Reset - barrier with',
     &        isize,' jobs'
        CALL pvmfbarrier ( cgroup, isize, info )
        IF ( info.ne.PvmOk ) THEN
          kinfo = CLIM_Group
          WRITE(nulprt, FMT='(A,I3,A)') 
     &          'Reset - pb barrier <pvm ',info,'>'
        ELSE
          CALL pvmflvgroup ( cgroup, info )
          IF ( info.ne.PvmOk ) THEN
            kinfo = CLIM_Group
            WRITE(nulprt, FMT='(A,I3,A)') 
     &            'Reset - pb lvgroup <pvm ',info,'>'
          ENDIF
        ENDIF
      ENDIF
c
c*    2. reset variables
c     ------------------
c
      nmods = 0
      WRITE (cgroup, FMT='(A8)') ' '
      WRITE (cmynam, FMT='(A32)') ' '
c
      nports = 0
      nlinks = 0
      nbsend = 0
      nbrecv = 0
      DO 10 ip=0,CLIM_MaxMod-1
        modtid(ip) = -1
        nstep(ip) = 0
        nfcpl(ip) = 1
        nstdt(ip) = 1
   10 CONTINUE
c
      nexit = 1
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      WRITE (nulprt, FMT='(A)') 'Reset - -'
      CALL FISH(nulprt)
      RETURN
      END
