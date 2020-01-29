      SUBROUTINE CLIM_Abort(klinfo,cdmsg)
c
c*    *** Abort ***   CLIM 2.0
c
c     purpose:
c     --------
c        leave pvm and abort
c
c     interface:
c     ----------
c        klinfo : last clim error code returned
c        cdmsg  : message to prompt before abort
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
      INTEGER       klinfo
      CHARACTER*(*) cdmsg
c     ----------------------------------------------------------------
      INTEGER*4     info, isize
      CHARACTER*40  clmsgo(CLIM_MaxCodes:0)
c     ----------------------------------------------------------------
c
      clmsgo(CLIM_Ok       ) = 'CLIM Ok'
      clmsgo(CLIM_FastExit ) = 'Fast exit due to previous error'
      clmsgo(CLIM_BadName  ) = 'Unknown model name'
      clmsgo(CLIM_BadPort  ) = 'Invalid port name'
      clmsgo(CLIM_BadType  ) = 'Mismatch in data types'
      clmsgo(CLIM_DoubleDef) = 'Double port definition'
      clmsgo(CLIM_NotStep  ) = 'Not a coupling step'
      clmsgo(CLIM_IncStep  ) = 'Incompatible time step'
      clmsgo(CLIM_IncSize  ) = 'Incompatible port size definitions'
      clmsgo(CLIM_NotClim  ) = 'Wait got an external CLIM message'
      clmsgo(CLIM_TimeOut  ) = 'Time out reached'
      clmsgo(CLIM_Pvm      ) = 'Misc PVM error - see trace file'
      clmsgo(CLIM_FirstCall) = 'Error at the first PVM call'
      clmsgo(CLIM_PbRoute  ) = 'Unable to set the route mechanism'
      clmsgo(CLIM_Group    ) = 'Error in a PVM group function'
      clmsgo(CLIM_BadTaskId) = 'Bad internal model id'
      clmsgo(CLIM_NoTask   ) = 'A remote model does not exist'
      clmsgo(CLIM_InitBuff ) = 'Unable to create a send buffer'
      clmsgo(CLIM_Pack     ) = 'Unable to pack the set of data'
      clmsgo(CLIM_Unpack   ) = 'Unable to unpack a set of data'
      clmsgo(CLIM_Down     ) = 'The Virtual Machine is probably down'
      clmsgo(CLIM_PvmExit  ) = 'Error when leaving PVM'
c
c*    0. First Check
c     --------------
c
      WRITE(nulprt, FMT='(A,A,A)') 'Abort - *** ',cdmsg,' ***'
      WRITE(nulprt, FMT='(A,A,A)') 'Abort - *** ',clmsgo(klinfo),' ***'
c
      IF ( nexit.ne.1 ) THEN
        WRITE(nulprt, FMT='(A)') 'Abort - FastExit set on'
      ELSE
        WRITE(nulprt, FMT='(A)') 'Abort - FastExit set off'
      ENDIF
c
c
c*    1. proper exit from pvm group
c     -----------------------------
c
      CALL pvmfgsize ( cgroup, isize )
c
      IF ( isize.LE.0 ) THEN
        WRITE(nulprt, FMT='(A,I3,A)') 
     &        'Abort - pb gsize <pvm ',isize,'>'
      ELSE
        WRITE(nulprt, FMT='(A,I3,A)') 'Abort - with ',isize,' jobs'
      ENDIF
c
      CALL pvmflvgroup ( cgroup, info )
      IF ( info.NE.PvmOk ) THEN
        WRITE(nulprt, FMT='(A,I3,A)') 
     &        'Abort - pb lvgroup <pvm ',info,'>'
      ENDIF
c
c*    2. exit from pvm
c     ----------------
c
      CALL pvmfexit ( info )
      WRITE(nulprt, FMT='(A,I3,A)') 
     &      'Abort - exit status <pvm ',info,'>'
c
c*    3. abort
c     --------
c
      CALL ABORT
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      CALL FISH(nulprt)
      RETURN
      END
