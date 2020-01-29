      SUBROUTINE CLIM_Trace(ksend,krecv,kinfo)
c
c*    *** Trace ***   CLIM 2.0
c
c     purpose:
c     --------
c        returns historical data
c
c     interface:
c     ----------
c        ksend  : number of bytes out
c        krecv  : number of bytes in
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
      INTEGER   ksend, krecv, kinfo
c     ----------------------------------------------------------------
c
      ksend = nbsend
      krecv = nbrecv
      kinfo = CLIM_Ok
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      RETURN
      END
