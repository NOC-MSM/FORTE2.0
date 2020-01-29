      SUBROUTINE CLIM_Start(kmxtag,kinfo)
c
c*    *** Start ***   CLIM 2.0
c
c     purpose:
c     --------
c        beginning of the coupled run
c
c     interface:
c     ----------
c        kmxtag : maximum tag that should be used in the outside world
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
      INTEGER kmxtag, kinfo
c     ----------------------------------------------------------------
      CHARACTER*32 clrport(CLIM_MaxPort)
      CHARACTER*10 clsleep
      INTEGER*4    ip, info, ibuf, isize, ilgdt, ipos, is,
     &             itag1, itag2, ielap, isdmod, incp, ipotag
      INTEGER*4    ircode, irempo, irport(5,CLIM_MaxPort)
      INTEGER*4    irdist(CLIM_ParSize,CLIM_MaxPort)
c     ----------------------------------------------------------------
c
c*    0. First Check
c     --------------
c
      IF ( nexit.ne.1 ) THEN
        kinfo = CLIM_FastExit
        WRITE(nulprt, FMT='(A)') 'Start - should not be called'
        GO TO 1010
      ENDIF
      kinfo  = CLIM_Ok
      kmxtag = CLIM_MaxTag - 1
      nexit  = 0
c
c*    1. join pvm group
c     -----------------
c
      CALL pvmfjoingroup ( cgroup, mynum )
      IF ( mynum.lt.0 ) THEN
        kinfo = CLIM_Group
        WRITE (nulprt, FMT='(A,I3,A)') 
     &         'Start - join group pb <pvm ',mynum,'>'
        GO TO 1010
      ELSE
        modtid(mynum) = mytid
        WRITE (nulprt, FMT='(A,I4)') 'Start - join group no:',mynum
      ENDIF
c
      ielap = 0
      WRITE (clsleep, FMT='(A,1X,I4)') 'sleep', ntiret
      WRITE (nulprt, FMT='(A,A)') 'Start - system call: ', clsleep
c
  110 CONTINUE
      CALL pvmfgsize ( cgroup, isize )
      IF ( isize.lt.0 ) THEN
        kinfo = CLIM_Group
        WRITE(nulprt, FMT='(A,I3,A)') 
     &        'Start - pb size group <pvm ',isize,'>'
        GO TO 1010
      ELSEIF ( isize.lt.nmods ) THEN
        IF ( ielap.le.ntiogp ) THEN
#if defined(CRAY)
          CALL ISHELL ( clsleep )
#else
          CALL SYSTEM ( clsleep )
#endif
          ielap = ielap +  ntiret
          GO TO 110
        ELSE
          kinfo = CLIM_TimeOut
          WRITE (nulprt,FMT='(A,I2,A)') 
     &           'Start - time out to gather ',nmods,' models'
          GO TO 1010
        ENDIF
      ENDIF
c
c*    2. broadcast usefull informations
c     ---------------------------------
c
      itag1 = CLIM_MaxTag
      ilgdt = CLIM_ParSize
      CALL pvmfinitsend ( PVMDATADEFAULT, ibuf )
      CALL pvmfpack ( INTEGER4, mynum, 1, 1, info )
      CALL pvmfpack ( STRING, cmynam, CLIM_Clength, 1, info )
      CALL pvmfpack ( INTEGER4, mytid, 1, 1, info )
      CALL pvmfpack ( INTEGER4, mystep, 1, 1, info )
      CALL pvmfpack ( INTEGER4, mystdt, 1, 1, info )
      CALL pvmfpack ( INTEGER4, myfcpl, 1, 1, info )
      CALL pvmfpack ( INTEGER4, mycode, 1, 1, info )
      CALL pvmfpack ( INTEGER4, nports, 1, 1, info )
      DO 210 ji=1,nports
        CALL pvmfpack ( STRING, cports(ji), CLIM_Clength, 1, info )
        CALL pvmfpack ( INTEGER4, myport(1,ji), 5, 1, info )
        CALL pvmfpack ( INTEGER4, mydist(1,ji), ilgdt, 1, info )
  210 CONTINUE
      CALL pvmfbcast ( cgroup, itag1, info )
      WRITE(nulprt, FMT='(A,I3,A)') 
     &      'Start - broadcast complete <pvm ',info,'>'
c
c*    3. get these infos from other models and check ports
c     ----------------------------------------------------
c
      DO 380 ip=1,nmods-1
c
         CALL pvmftrecv ( -1, itag1, ntiout, 0, ibuf )
         IF ( ibuf.le.0 ) THEN
           kinfo = CLIM_TimeOut
           WRITE(nulprt, FMT='(A,I3,A)') 
     &            'Start - abnormal exit from trecv <pvm ',ibuf,'>'
           GO TO 1010
         ENDIF
         CALL pvmfunpack ( INTEGER4, isdmod, 1, 1, info )
         CALL pvmfunpack ( STRING, cnames(isdmod), 
     &                             CLIM_Clength, 1, info )
         CALL pvmfunpack ( INTEGER4, modtid(isdmod), 1, 1, info )
         CALL pvmfunpack ( INTEGER4, nstep(isdmod), 1, 1, info )
         CALL pvmfunpack ( INTEGER4, nstdt(isdmod), 1, 1, info )
         CALL pvmfunpack ( INTEGER4, nfcpl(isdmod), 1, 1, info )
         CALL pvmfunpack ( INTEGER4, ircode, 1, 1, info )
         CALL pvmfunpack ( INTEGER4, irempo, 1, 1, info )
         DO 310 ji=1,irempo
           CALL pvmfunpack ( STRING, clrport(ji), 
     &                               CLIM_Clength, 1, info )
           CALL pvmfunpack ( INTEGER4, irport(1,ji), 5, 1, info )
           CALL pvmfunpack ( INTEGER4, irdist(1,ji), ilgdt, 1, info )
  310    CONTINUE
         WRITE (nulprt, FMT='(A,A)') 'Start - MODEL ', cnames(isdmod)
         WRITE (nulprt, FMT='(A,I9)') 'Start -   num  :', isdmod
         WRITE (nulprt, FMT='(A,I9)') 'Start -   tid  :', modtid(isdmod)
         WRITE (nulprt, FMT='(A,I9)') 'Start -   nstep:', nstep(isdmod)
         WRITE (nulprt, FMT='(A,I9)') 'Start -   nstdt:', nstdt(isdmod)
         WRITE (nulprt, FMT='(A,I9)') 'Start -   nfcpl:', nfcpl(isdmod)
         WRITE (nulprt, FMT='(A,I9)') 'Start -   myport:', myport(4,1)
         WRITE (nulprt, FMT='(A,I9)') 'Start -   irport:', irport(4,1)
c
         IF ( mycode.eq.ircode ) THEN
c          ncode(isdmod) = PVMDATAINPLACE
           ncode(isdmod) = PVMDATADEFAULT
         ELSE
           ncode(isdmod) = PVMDATADEFAULT
         ENDIF
         WRITE(nulprt, FMT='(A,I9)') 'Start -   datap:', ncode(isdmod)
c
         DO 360 ji=1,nports
         DO 350 jj=1,irempo
           IF ( cports(ji).eq.clrport(jj) .and.
     &          myport(1,ji)+irport(1,jj).eq.1) THEN
             IF ( (mydist(1,ji).eq.irdist(1,jj) .and.
     &             myport(4,ji).ne.irport(4,jj)).or.
     &            (mydist(CLIM_Strategy,ji).eq.CLIM_Serial .and.
     &             myport(4,ji).lt.irport(4,jj)).or.
     &            (irdist(CLIM_Strategy,jj).eq.CLIM_Serial .and.
     &             irport(4,jj).lt.myport(4,ji))) THEN
               kinfo = CLIM_IncSize
               WRITE(nulprt, FMT='(A,A,A,I2,A,I2,A,I2,I2)')
     &               'Start - WARNING Incompatible sizes - field',
     &               cports(ji),'model ',mynum,' and model ',
     &               isdmod,': ',myport(4,ji),irport(4,jj)
             ELSEIF ( myport(2,ji).ne.irport(2,jj) ) THEN
               kinfo = CLIM_BadType
               WRITE(nulprt, FMT='(A,A,A,I2,A,I2,A,I2,I2)')
     &               'Start - WARNING Incompatible types - field',
     &               cports(ji),'model ',mynum,' and model ',
     &               isdmod,': ',myport(2,ji),irport(2,jj)
             ELSE
               IF ( myport(1,ji).eq.CLIM_Out ) THEN
                 incp   = mystep / myfcpl + 1
                 ipotag = CLIM_MaxTag - (ji-1)*incp - 1
               ELSE
                 incp   = nstep(isdmod) / nfcpl(isdmod) + 1
                 ipotag = CLIM_MaxTag - (jj-1)*incp - 1
               ENDIF
               IF ( (ipotag-incp).lt.kmxtag ) THEN
                 kmxtag = ipotag - incp
               ENDIF
               nlinks = nlinks + 1
               ipos   = 5
               myport(ipos,ji) = myport(ipos,ji) + 1
               myport(ipos+myport(ipos,ji),ji) = nlinks
               mylink(1,nlinks) = isdmod
               mylink(2,nlinks) = modtid(isdmod)
               mylink(3,nlinks) = ipotag
               mylink(4,nlinks) = 1
               mylink(5,nlinks) = 0
               mylink(6,nlinks) = myport(4,ji)
               IF ( mydist(CLIM_Strategy,ji).eq.CLIM_Serial .and.
     &              irdist(CLIM_Strategy,jj).ne.CLIM_Serial) THEN
                 mylink(4,nlinks) = irdist(CLIM_Segments,jj)
                 DO 320 is=1,2*irdist(CLIM_Segments,jj)
                   mylink(4+is,nlinks)=irdist(CLIM_Segments+is,jj)
  320            CONTINUE
               ENDIF
               WRITE(nulprt, FMT='(A,A)')
     &               'Start - LINK ',cports(ji)
               WRITE(nulprt, FMT='(A,I2,A,I1,A,I2,A,I1,A,I9,A,I9)')
     &               'Start - [model ',
     &               mynum,'/io ',myport(1,ji),'] - [model ',
     &               isdmod,'/io ',irport(1,jj),'] using tags ',
     &               ipotag,' to ',ipotag-incp+1
             ENDIF
           ENDIF
  350    CONTINUE
  360    CONTINUE
c
         WRITE(nulprt, FMT='(A)') 'Start - -'
c        ENDIF
  380 CONTINUE
c
c*    4. Get wall clock differences with other models
c     -----------------------------------------------
c
      DO 410 j=0,CLIM_MaxMod-1
        IF ( modtid(j).gt.0 .and. j.ne.mynum ) THEN
          itag1 = mynum*CLIM_MaxMod + j
          itag2 = j*CLIM_MaxMod + mynum
          IF ( j.gt.mynum ) THEN
             CALL CLIM_Hostdt(modtid(j),itag1,delta(j),delte(j))
             CALL pvmfinitsend ( PVMDATADEFAULT, info )
             CALL pvmfpack ( REAL8, delta(j), 1, 1, info )
             CALL pvmfpack ( REAL8, delte(j), 1, 1, info )
             CALL pvmfsend ( modtid(j), itag2, info )
          ELSE
             CALL CLIM_Nodedt ( modtid(j), itag2 )
             CALL pvmfrecv ( modtid(j), itag1, info )
             CALL pvmfunpack ( REAL8, delta(j), 1, 1, info )
             CALL pvmfunpack ( REAL8, delte(j), 1, 1, info )
             delta(j) = -1.0*delta(j)
          ENDIF
        ENDIF
  410 CONTINUE
c
c*    correct execution
c
      nexit = 1
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      CALL FISH(nulprt)
      RETURN
      END
