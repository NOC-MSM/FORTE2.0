      SUBROUTINE CLIM_Wait(cdport,kstep,kinfo)
c
c*    *** Wait ***   CLIM 2.0
c
c     purpose:
c     --------
c        wait for any message on any port
c
c     interface:
c     ----------
c        cdport : output port
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
      CHARACTER*(*) cdport
      INTEGER       kstep, kinfo
c     ----------------------------------------------------------------
      INTEGER*4     info, ibuf, ilen, itag, itid, ip, il, ilink
      REAL*8        wallclk, zt0
      EXTERNAL      wallclk
c     ----------------------------------------------------------------
c
c*    0. First Check
c     --------------
c
      IF ( nexit.ne.1 ) THEN
        kinfo = CLIM_FastExit
        WRITE(nulprt, FMT='(A)') 'Wait - should not be called'
        GO TO 1010
      ENDIF
      kinfo = CLIM_Ok
c
c*    1. probe for any message
c     ------------------------
c
      iret = 0
      zt0 = wallclk()
c
  100 CONTINUE
c
      CALL pvmfprobe ( -1, -1, ibuf )
      IF ( ibuf.lt.0 ) THEN
        kinfo = CLIM_Down
        WRITE(nulprt,FMT='(A,I3,A)')
     &        'Wait - abnormal exit from probe <pvm ',ibuf,'>'
        nexit = 0
        GO TO 1010
      ELSEIF ( ibuf.eq.0 ) THEN
        iret = iret + 1
        IF ( mod(iret,50).eq.0 ) THEN
          IF ( (wallclk() - zt0).gt.ntiout ) THEN
            kinfo = CLIM_TimeOut
            GO TO 1010
          ENDIF
        ENDIF
        GO TO 100
      ENDIF
c
      CALL pvmfbufinfo ( ibuf, ilen, itag, itid, info )
c
c*    2. check for the associated port and step
c     -----------------------------------------
c
      kinfo = CLIM_NotClim
      kstep = CLIM_Void
      WRITE(cdport, FMT='(A32)') 'NONE'
c
      DO 220 ip=1,nports
        IF ( myport(1,ip).eq.CLIM_In ) THEN
          DO 210 il=1,myport(5,ip)
            ilink = myport(5+il,ip)
            inum  = mylink(1,ilink)
            incp  = nstep(inum) / nfcpl(inum) + 1
            IF ( mylink(2,ilink).eq.itid .and.
     &           mylink(3,ilink).ge.itag .and.
     &           mylink(3,ilink)-incp+1.le.itag ) THEN
              kinfo = CLIM_Ok
              kstep = mylink(3,ilink) - itag
              WRITE(cdport, FMT='(A32)') cports(ip)
            ENDIF
  210     CONTINUE
        ENDIF
  220 CONTINUE
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      CALL FISH(nulprt)
      RETURN
      END
