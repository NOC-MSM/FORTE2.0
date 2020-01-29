      SUBROUTINE CLIM_Export(cdport,kstep,pfield,kinfo)
c
c*    *** Export ***   CLIM 2.0
c
c     purpose:
c     --------
c        give pfield to models connected to port cdport at the step kstep
c
c     interface:
c     ----------
c        cdport : port character string
c        kstep  : current time step
c        pfield : buffer of reals
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
      CHARACTER     pfield(1)
c     ----------------------------------------------------------------
      CHARACTER*32 clport
      INTEGER*4    i4kstep
      INTEGER*4    ibuf, info
      INTEGER*4    isend, ip, iport, ilk, iseg, is, ilgb,
     *             imod, itid, itag, ilen, ioff, ityp, ibyt
c     ----------------------------------------------------------------
c
c*    0. First Check
c     --------------
c
      i4kstep = kstep
c
      IF ( nexit.ne.1 ) THEN
        kinfo = CLIM_FastExit
        WRITE(nulprt, FMT='(A)') 'Export - should not be called'
        GO TO 1010
      ENDIF
      kinfo = CLIM_Ok
c
c*    1. check for this port in my list
c     ---------------------------------
c
      isend =  0
      iport = -1
      WRITE(clport, FMT='(A32)') cdport
c
      DO 100 ip=1,nports
        IF (clport.eq.cports(ip) .and. myport(1,ip).eq.CLIM_Out) THEN
          iport=ip
          GO TO 110
        ENDIF
  100 CONTINUE
c
  110 CONTINUE
      IF ( iport.lt.0 ) THEN
        kinfo = CLIM_BadPort
        WRITE(nulprt,FMT='(A,A)')
     *        'Export - WARNING - Invalid port out: ',cdport
        GO TO 1010
      ELSEIF ( mod(i4kstep,myfcpl).ne.0 ) THEN
        kinfo = CLIM_NotStep
        WRITE(nulprt,FMT='(A,I8)') 
     *        'Export - WARNING - not a coupling step:',kstep
        GO TO 1010
      ENDIF
c
c*    2. check for connected ports (in)
c     ---------------------------------
c
      WRITE(nulprt, FMT='(A,A)') 'Export - ', cdport
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
     *          'Export - WARNING - incompatible coupling step:',
     *           kstep,' with model ',cnames(imod)
c
        ELSE

          CALL pvmfpstat ( itid, istat )

            IF ( istat.eq.PvmOk ) THEN
              CALL pvmfinitsend ( ncode(imod), ibuf )
              IF (ibuf.lt.PvmOk) THEN
                kinfo = CLIM_InitBuff
                WRITE(nulprt, FMT='(A,I3,A)') 
     *                'Export - pb initsend <pvm ',ibuf,'>'
              ENDIF
              ilgb = 0
              DO 210 is=1,iseg
                ioff = mylink(4+2*is-1,ilk) * ibyt + 1
                ilen = mylink(4+2*is,ilk)
                CALL pvmfpack ( ityp, pfield(ioff), ilen, 1, info )
                ilgb = ilgb + ilen
  210         CONTINUE
              IF (info.ne.PvmOk) THEN
                kinfo = CLIM_Pack
                WRITE(nulprt, FMT='(A,I3,A)') 
     *                'Export - pb pack<pvm ',info,'>'
              ELSE
                CALL pvmfsend ( itid, itag, info )
                IF ( info.eq.PvmOk ) THEN
                  isend = isend + 1
                  nbsend = nbsend + ilgb * ibyt
                  WRITE(nulprt, FMT='(A,I2,A,I4,A,I7,A,I2,A,I9,A)') 
     *                  'Export - <dest:',imod,
     *                  '> <step:',kstep,
     *                  '> <len:',ilgb,
     *                  '> <type:',ibyt,
     *                  '> <tag:',itag,'>'
                ELSE
                  kinfo = CLIM_Pvm
                  WRITE(nulprt, FMT='(A,I3,A)') 
     *                  'Export - pb send <pvm ',info,'>'
                ENDIF   
              ENDIF
            ELSEIF ( istat.eq.PvmNoTask ) THEN
              kinfo = CLIM_NoTask
              nexit = 0
            ELSEIF ( istat.eq.PvmBadParam ) THEN
              kinfo = CLIM_BadTaskId
              nexit = 0
            ELSE
              kinfo = CLIM_Down
              nexit = 0
            ENDIF
c
         ENDIF
c
  290 CONTINUE
c
      WRITE(nulprt, FMT='(A,I3,A)') 
     *     'Export - ',isend,' fields exported'
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      CALL FISH(nulprt)
      RETURN
      END
