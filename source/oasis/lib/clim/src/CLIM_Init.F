      SUBROUTINE CLIM_Init(cdexp,cdnam,kno,ktrout,
     &                     kstep,kfcpl,kdt,
     &                     ktiret,ktiogp,ktiout,kinfo)
c
c*    *** Init ***   CLIM 2.0
c
c     purpose:
c     --------
c        start one model
c
c     interface:
c     ----------
c        cdexp  : experience descriptor
c        cdnam  : name of the model
c        kno    : number of applications in the coupling
c        ktrout : unit number for trace file
c        kstep  : number of step in the simulation
c        kfcpl  : frequency of coupling in steps
c        kdt    : length of a time step in seconds
c        ktiret : time in second between two Pvm requests
c        ktiogp : total time out in second to gather models
c        ktiout : total time out in second to receive a message
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
      CHARACTER*(*) cdexp, cdnam
      INTEGER       kno, ktrout, kstep, kdt, kfcpl,
     &              ktiret, ktiogp, ktiout, kinfo
c     ----------------------------------------------------------------
      INTEGER       iost
c     ----------------------------------------------------------------
      INTEGER*4     ip, info
      INTEGER*4     itask, itid, iptid, mydtid, idtid, iflag,
     &              inhost, iarch, ispeed
      CHARACTER*10  claout, clhost, clarch
c     ----------------------------------------------------------------
c
c*    0. get arguments and some initilizations
c     ----------------------------------------
c
      kinfo = CLIM_Ok
      nexit = 0
c
      nmods = kno
      WRITE(cgroup, FMT='(A8)') cdexp
      WRITE(cmynam, FMT='(A32)') cdnam
c
      IF ( ktiret.lt.0 ) THEN
        ntiret = 1
      ELSE
        ntiret = ktiret
      ENDIF
c
      IF ( ktiogp.le.0 ) THEN
        ntiogp = 86400
      ELSE
        ntiogp = ktiogp
      ENDIF
c
      IF ( ktiout.le.0 ) THEN
        ntiout = 86400
      ELSE
        ntiout = ktiout
      ENDIF
c
      mystep = kstep
c
      IF ( kdt.eq.CLIM_Void ) THEN
        mystdt = 1
      ELSE
        mystdt = kdt
      ENDIF
c
      IF ( kfcpl.eq.CLIM_Void ) THEN
        myfcpl = 1
      ELSE
        myfcpl = kfcpl
      ENDIF
c
      nports = 0
      nlinks = 0
      nbsend = 0
      nbrecv = 0
      DO 10 ip=0,CLIM_MaxMod-1
        modtid(ip) = -1
        nstep(ip)  =  0
        nfcpl(ip)  =  1
        nstdt(ip)  =  1
   10 CONTINUE
c
c*    1. open trace file
c     ------------------
c
      iost = 0
      nulprt = ktrout
      WRITE(cnaprt, FMT='(A,''.prt'')') cdnam
      OPEN(UNIT=nulprt, FILE=cnaprt, STATUS='UNKNOWN',
     &     FORM='FORMATTED', ERR=110, IOSTAT=iost)
c
  110 CONTINUE
      IF ( iost.ne.0 ) THEN
        WRITE(6,*) 'ABORT in Init - unable to open trace file ', iost
        WRITE(6,*) nulprt, ' ', cnaprt
        CALL ABORT
      ENDIF
c
      WRITE(nulprt, FMT='(A)')   'Init - CLIM 2.0 / pvm3.3'
      WRITE(nulprt, FMT='(A,A)') 'Init - experiment name: ', cgroup
      WRITE(nulprt, FMT='(A,A)') 'Init - name of the model: ', cdnam
c
c*    2. join pvm and set first options
c     ---------------------------------
c
      CALL pvmfmytid ( mytid )
      IF ( mytid.lt.0 ) THEN
        kinfo = CLIM_FirstCall
        WRITE(nulprt, FMT='(A,I4)') 'Init - pb starting pvm:', mytid
        GO TO 1010
      ELSE
        WRITE(nulprt, FMT='(A,I9)') 'Init - my tid:', mytid
      ENDIF
c
      IF ( kno.lt.32 ) THEN
        CALL pvmfsetopt ( PVMROUTE, PVMROUTEDIRECT, info )
        IF ( info.lt.PvmOk ) THEN
          kinfo = CLIM_PbRoute
          WRITE(nulprt, FMT='(A,I3)') 'Init - pb route option:', info
          GO TO 1010
        ELSE
          WRITE(nulprt, FMT='(A,I4)') 'Init - direct route on:', info
        ENDIF
      ENDIF
c
c*    3. Get architecture and data representation
c     -------------------------------------------
c
      CALL pvmftasks ( mytid, itask, itid, iptid, mydtid,
     &                 iflag, claout, info )
c
      inhost = 1
      DO 300 ip=1,inhost
        CALL pvmfconfig ( inhost, iarch, idtid, clhost, clarch,
     &                    ispeed, info )
        IF ( idtid.eq.mydtid ) THEN
          IF (iarch.gt.1) THEN
            IF (clarch(1:4).eq.'CRAY') THEN
              mycode = 2
            ELSE
              mycode = 1
            ENDIF
          ELSE
            mycode = 0
          ENDIF
          WRITE(nulprt, FMT='(A,A)') 'Init - host name: ', clhost
          WRITE(nulprt, FMT='(A,A)') 'Init - architecture: ', clarch
        ENDIF
  300 CONTINUE
c
c*    correct execution
c
      nexit = 1
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      WRITE(nulprt, FMT='(A)') 'Init - -'
      CALL FISH(nulprt)
      RETURN
      END
