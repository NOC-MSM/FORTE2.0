      SUBROUTINE CLIM_Time(ddt,kinfo)
c
c*    *** Time ***   CLIM 2.0
c
c     purpose:
c     --------
c        returns elapsed time in seconds
c
c     interface:
c     ----------
c        ddt    : output double real time in seconds
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
      INTEGER   kinfo
      REAL*8    ddt, wallclk
      EXTERNAL  wallclk
c     ----------------------------------------------------------------
c
      ddt = wallclk()
      kinfo = CLIM_Ok
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      RETURN
      END
