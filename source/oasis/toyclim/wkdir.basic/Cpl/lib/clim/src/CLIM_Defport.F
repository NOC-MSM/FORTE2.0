c**** Subroutine of CLIM_Define
c**** Should not be CALLed by user
c
      SUBROUTINE CLIM_Defport(cdport,kinout,ktype,kparal,kinfo)
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
#include "fpvm3.h"
#include "clim.h"
#include "comclim.h"
c     ----------------------------------------------------------------
      INTEGER       kinout, ktype, kparal(10), kinfo
      CHARACTER*(*) cdport
c     ----------------------------------------------------------------
      INTEGER*4     ip, is
      CHARACTER*32  cltest
c     ----------------------------------------------------------------
c
c*    1. check if this port already exist
c     -----------------------------------
c
      WRITE(cltest, FMT='(A32)') cdport
c
      DO 100 ip=1,nports
        IF (cltest.eq.cports(ip) .and. kinout.eq.myport(1,ip)) THEN
          kinfo = CLIM_DoubleDef
          WRITE(nulprt, FMT='(A,A)') 
     &          'Define - WARNING - duplicate definition of port ', 
     &           cdport
          GO TO 1010
        ENDIF
  100 CONTINUE
c
c*    2. save arguments as half a link
c     --------------------------------
c
      nports = nports + 1
      WRITE(cports(nports), FMT='(A32)') cdport
c
      myport(1,nports) = kinout
      myport(5,nports) = 0
c
      IF ( ktype.eq.CLIM_Integer ) THEN
        myport(2,nports) = INTEGER4
        myport(3,nports) = 4
      ELSEIF ( ktype.eq.CLIM_Real ) THEN
        myport(2,nports) = REAL4
        myport(3,nports) = 4
      ELSEIF ( ktype.eq.CLIM_Double ) THEN
        myport(2,nports) = REAL8
        myport(3,nports) = 8
      ELSE
        kinfo = CLIM_BadType
        WRITE(nulprt, FMT='(A,I4)') 
     &        'Define - WARNING - Bad data type:',ktype
      ENDIF
c
      IF (kparal(CLIM_Strategy).eq.CLIM_Serial) THEN
c
        mydist(CLIM_Strategy,nports)   = CLIM_Serial
        mydist(CLIM_Segments,nports)   = 1
        mydist(CLIM_Segments+1,nports) = 0
        mydist(CLIM_Segments+2,nports) = kparal(CLIM_Length)
        myport(4,nports) = kparal(CLIM_Length)
c
      ELSEIF (kparal(CLIM_Strategy).eq.CLIM_Apple) THEN
c
        mydist(CLIM_Strategy,nports)   = CLIM_Apple
        mydist(CLIM_Segments,nports)   = 1
        mydist(CLIM_Segments+1,nports) = kparal(CLIM_Offset)
        mydist(CLIM_Segments+2,nports) = kparal(CLIM_Length)
        myport(4,nports) = kparal(CLIM_Length)
c
      ELSEIF (kparal(CLIM_strategy).eq.CLIM_Box) THEN
c
        mydist(CLIM_Strategy,nports)   = CLIM_Box
        mydist(CLIM_Segments,nports)   = kparal(CLIM_SizeY)
        DO 210 is=1,kparal(CLIM_SizeY)
          mydist(CLIM_Segments+2*is-1,nports) = 
     &           kparal(CLIM_Offset) + (is-1) * kparal(CLIM_LdX)
          mydist(CLIM_Segments+2*is,nports) = kparal(CLIM_SizeX)
  210   CONTINUE
        myport(4,nports) = kparal(CLIM_SizeX) * kparal(CLIM_SizeY)
c
      ELSEIF (kparal(CLIM_strategy).eq.CLIM_Orange) THEN
c
        mydist(CLIM_Strategy,nports)   = CLIM_Box
        mydist(CLIM_Segments,nports)   = kparal(CLIM_Segments)
        myport(4,nports) = 0
        DO 220,is=1,2*kparal(CLIM_Segments)
          mydist(CLIM_Segments+is,nports) = kparal(CLIM_Segments+is)
          IF (mod(is,2).eq.0) THEN
            myport(4,nports) = 
     &        myport(4,nports) + kparal(CLIM_Segments+is)
          ENDIF
  220   CONTINUE
c
      ENDIF
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      CALL FISH(nulprt)
      RETURN
      END
