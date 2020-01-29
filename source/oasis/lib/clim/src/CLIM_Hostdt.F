      SUBROUTINE CLIM_Hostdt(ktid,ktag,ddt,dde)
c
c*    *** Abort ***   CLIM 2.0
c
c     purpose:
c     --------
c        get processes *dt*
c        dt is the time difference in seconds between 
c        distributed CALLs to getimeofday.
c
c     interface:
c     ----------
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
c     ----------------------------------------------------------------
      INTEGER     ktid, ktag
      REAL*8      ddt, dde
c     ----------------------------------------------------------------
      INTEGER*4   nm, j, mytid, ibuf, info
      PARAMETER   ( nm = 3 )
      REAL*8      t0, t1, t2, dt, diff, wallclk
      EXTERNAL    wallclk
c     ----------------------------------------------------------------
c
      CALL pvmfmytid ( mytid )
      CALL pvmfinitsend ( PVMDATADEFAULT, info )
      CALL pvmfpack ( INTEGER4, mytid, 1, 1, info )
      CALL pvmfsend ( ktid, ktag, info )
      CALL pvmfrecv ( ktid, ktag, ibuf )
c
      diff=1.0E+99
      DO 110 j=1,nm
        t0 = wallclk()
        CALL pvmfinitsend ( PVMDATADEFAULT, info )
        CALL pvmfpack ( REAL8, t0, 1, 1, info )
        CALL pvmfsend ( ktid, ktag, info )
        CALL pvmfrecv ( ktid, ktag, ibuf )
        t2 = wallclk()
        CALL pvmfunpack ( REAL8, t1, 1, 1, info )
        dt = (t0+t2)/2.0 - t1
        IF ( t2-t0.lt.diff ) THEN
          diff = t2-t0
          ddt  = dt
        ENDIF
  110 CONTINUE
c
      dde = diff
c
      RETURN
      END
