      SUBROUTINE CLIM_Define(cdport,kinout,ktype,kparal,kinfo)
c
c*    *** Define ***   CLIM 2.0
c
c     purpose:
c     --------
c        define a port
c
c     interface:
c     ----------
c        cdport : symnolic name of the field
c        kinout : port status in/out
c        ktype  : type of data
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
      INTEGER       kinout, ktype, kparal(10), kinfo
      CHARACTER*(*) cdport
c     ----------------------------------------------------------------
c
c*    0. First Check
c     --------------
c
      IF ( nexit.ne.1 ) THEN
        kinfo = CLIM_FastExit
        WRITE(nulprt,FMT='(A)') 'Define - should not be called'
        GO TO 1010
      ENDIF
      kinfo = CLIM_Ok
c
c*    1. define the port
c     ------------------
c
      IF ( kinout.eq.CLIM_InOut ) THEN
        CALL CLIM_Defport ( cdport, CLIM_In,  ktype, kparal, kinfo )
        IF (kinfo.eq.CLIM_Ok) THEN
          CALL CLIM_Defport ( cdport, CLIM_Out, ktype, kparal, kinfo )
        ENDIF
      ELSE
        CALL CLIM_Defport ( cdport, kinout,  ktype, kparal, kinfo )
      ENDIF
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      CALL FISH(nulprt)
      RETURN
      END
