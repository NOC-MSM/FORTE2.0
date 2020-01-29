      SUBROUTINE CLIM_Import(cdport,kstep,pfield,kinfo)
c
c*    *** Import ***   CLIM 2.0
c
c     purpose:
c     --------
c        recv pfield from models connected to port cdport
c
c     interface:
c     ----------
c        cdport  : port character string
c        kstep   : current time step
c        pfield  : buffer of reals
c        kinfo   : output status
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
      CHARACTER     pfield(1)
c     ----------------------------------------------------------------
      CHARACTER*32  clport
      INTEGER*4     i4kstep
      INTEGER*4     ibuf, info
      INTEGER*4     irecv, imod, ilk, iseg, is, ilgb,
     &              itid, itag, ilen, ioff, ityp, ibyt
c     ----------------------------------------------------------------
c
c*    0. First Check
c     --------------
c
      i4kstep = kstep
c
      IF ( nexit.ne.1 ) THEN
        kinfo = CLIM_FastExit
        WRITE(nulprt, FMT='(A)') 'Import - should not be called'
        GO TO 1010
      ENDIF
      kinfo = CLIM_Ok
c
c*    1. check for this port in my list
c     ---------------------------------
c
      irecv =  0
      iport = -1
      WRITE(clport, FMT='(A32)') cdport
c
      DO 100 ip=1,nports
        IF ( clport.eq.cports(ip) .and. myport(1,ip).eq.CLIM_In ) THEN
          iport = ip
          GO TO 110
        ENDIF
  100 CONTINUE
c
  110 CONTINUE
      IF ( iport.lt.0 ) THEN
        kinfo = CLIM_BadPort
        WRITE(nulprt, FMT='(A,A)')
     &        'Import - WARNING - Invalid port out: ', cdport
        GO TO 1010
      ELSEIF ( mod(i4kstep,myfcpl).ne.0 ) THEN
        kinfo = CLIM_NotStep
        WRITE(nulprt, FMT='(A,I8)')
     &        'Import - WARNING - not a coupling step:', kstep
        GO TO 1010
      ENDIF
c
c*    2. check for connected ports (in)
c     ---------------------------------
c
      WRITE(nulprt,FMT='(A,A)') 'Import - ', cdport
c
      ityp = myport(2,iport)
      ibyt = myport(3,iport)
c
      DO 290 ip=1,myport(5,iport)
c
        ilk  = myport(5+ip,iport)
        imod = mylink(1,ilk)
        itid = mylink(2,ilk)
        itag = mylink(3,ilk) - i4kstep / myfcpl
        iseg = mylink(4,ilk)
c
        IF ( mod(i4kstep*mystdt,nfcpl(imod)*nstdt(imod)).ne.0 ) THEN
          kinfo = CLIM_IncStep
          WRITE(nulprt, FMT='(A,I8,A,A)')
     &          'Import - WARNING - incompatible coupling step:',
     &          kstep,' with model ',cnames(imod)
c
        ELSE
c
          CALL pvmfpstat ( itid, info )
          IF (info.eq.PvmNoTask) THEN
            kbuf = CLIM_NoTask
            GO TO 1010
          ELSEIF ( info.eq.PvmBadParam ) THEN
            kbuf = CLIM_BadTaskId
            GO TO 1010
          ELSEIF (info.lt.0) THEN
            kbuf = CLIM_Down
            GO TO 1010
          ENDIF
          CALL pvmftrecv ( itid, itag, ntiout, 0, ibuf )
          IF ( ibuf.gt.0 ) THEN
            ilgb = 0
            DO 210 is=1,iseg
              ioff = mylink(4+2*is-1,ilk) * ibyt + 1
              ilen = mylink(4+2*is,ilk)
              CALL pvmfunpack ( ityp, pfield(ioff), ilen, 1, info )
              ilgb = ilgb + ilen
  210       CONTINUE
            IF (info.eq.PvmOk) THEN
              irecv  = irecv + 1
              nbrecv = nbrecv + ilgb * ibyt
              WRITE(nulprt, FMT='(A,I2,A,I4,A,I7,A,I2,A,I9,A)') 
     &              'Import - <from:',imod,
     &              '> <step:',kstep,
     &              '> <len:',ilgb,
     &              '> <type:',ibyt,
     &              '> <tag:',itag,'>'
            ELSE
              kinfo = CLIM_Unpack
              WRITE(nulprt, FMT='(A,I3,A)') 
     &              'Import - pb unpack <pvm ',info,'>'
            ENDIF
          ELSE
            kinfo = CLIM_TimeOut
            WRITE(nulprt, FMT='(A,I3,A)') 
     &            'Import - abnormal exit from trecv <pvm ',ibuf,'>'
          ENDIF
        ENDIF
c
  290 CONTINUE
c
      WRITE(nulprt, FMT='(A,I3,A)')
     &     'Import - ',irecv,' fields imported'
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      CALL FISH(nulprt)
      RETURN
      END
