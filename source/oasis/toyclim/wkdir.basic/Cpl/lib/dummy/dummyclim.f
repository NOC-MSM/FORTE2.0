CCC
CCC     DUMMY SUBROUTINES FOR LINKING PURPOSE
CCC
      SUBROUTINE  CLIM_Quit( CLIM_StopPvm, kinfos)
C
      INTEGER CLIM_StopPvm
      INTEGER kinfos
C
      kinfos=1
C
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE CLIM_Start (kimxtag, kinfos)
C
      INTEGER kimxtag,kinfos
C
      kimxtag=kinfos
C
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE CLIM_Init (cdjobnam, cdoasis, kmodel, kultr, kiter,
     $                    kone , kstep, ktiret, ktiogp, ktiout,
     $                    knfos)
C
      CHARACTER*3 cdjobnam
      CHARACTER*5 cdoasis
      INTEGER kone , kstep, ktiret, ktiogp, ktiout
      INTEGER kmodel, kultr, kiter,knfos
C
      kone = kstep
      ktiret= ktiogp
      kmodel= kultr
      kiter=knfos
      ktiout=1
C
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE CLIM_Define (cdnaminp, CLIM_In, CLIM_Double,
     $          kiparal, kinfo)
C
      INTEGER kinfo, kiparal, CLIM_In, CLIM_Double
      CHARACTER*6 cdnaminp
c
      cdnaminp=' '
      kinfo= kiparal
C
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE CLIM_Stepi (cdmodnam, kstep, kfcpl, kdt, knfos)
C
      CHARACTER*6 cdmodnam
      INTEGER kstep, kfcpl, kdt, knfos
C
      cdmodnam=' '
      kstep=kfcpl
      kdt=knfos
C
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE CLIM_Import
     $              (cdlname, kiter, pfldold, kinfo)
C
      CHARACTER*8 cdlname
      INTEGER kiter,kinfo
      REAL pfldold
C
      pfldold=1.
      kiter=kinfo
      cdlname=' '
C
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE CLIM_Export
     $                (cdlname, kiter, pfldnew, kinfo)
C
      CHARACTER*8 cdlname
      INTEGER kiter,kinfo
      REAL pfldnew
C
      pfldnew=1.
      kiter=kinfo
      cdlname=' '
C
      RETURN
      END
C

