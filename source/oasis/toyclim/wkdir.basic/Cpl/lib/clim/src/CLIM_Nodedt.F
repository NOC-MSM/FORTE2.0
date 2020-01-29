      SUBROUTINE CLIM_Nodedt(ktid,ktag)
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
      INTEGER ktid, ktag
c     ----------------------------------------------------------------
      INTEGER*4   nm, j, hostid, info
      PARAMETER   ( nm = 3 ) 
      REAL*8      dlt1, wallclk
      EXTERNAL    wallclk
c     ----------------------------------------------------------------
c
      CALL pvmfrecv ( ktid, ktag, ibuf )
      CALL pvmfunpack ( INTEGER4, hostid, 1, 1, info )
      CALL pvmfinitsend ( PVMDATADEFAULT, info )
      CALL pvmfsend ( hostid, ktag, info )
c
      DO 110 j=1,nm
        CALL pvmfrecv ( hostid, ktag, ibuf )
        dlt1 = wallclk()
        CALL pvmfinitsend ( PVMDATADEFAULT, info )
        CALL pvmfpack ( REAL8, dlt1, 1, 1, info )
        CALL pvmfsend ( hostid, ktag, info )
  110 CONTINUE
c
      RETURN
      END
