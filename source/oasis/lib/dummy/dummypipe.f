CCC
CCC DUMMY ROUTINES FOR LINKING PURPOSE
CCC
CCC
      SUBROUTINE PIPE_Init_Model(cdmodnam,km)
c
      CHARACTER*6 cdmodnam
      INTEGER km
c
      cdmodnam=' '
      km=1
c
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE PIPE_Define_Model_Read(cdfread_sst,cdpread_sst,kinfo)
C
      CHARACTER*8 cdfread_sst
      CHARACTER*8 cdpread_sst
      INTEGER kinfo
C
      cdfread_sst=' '
      cdpread_sst=' '
      kinfo=1
c
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE PIPE_Define_Model_Write(cdfwrite,cdpwrite,kinfo)
C
      CHARACTER*8 cdpwrite
      CHARACTER*8 cdfwrite
      INTEGER kinfo
C
      cdpwrite=' '
      cdfwrite=' '
      kinfo=1
c
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE PIPE_Init (cdjobnam, cdmodnam, nmodel, infos)
C
      CHARACTER*6 cdmodnam
      CHARACTER*3 cdjobnam
      INTEGER nmodel,infos
C
      cdmodnam=' '
      cdjobnam=' '
      nmodel=infos
c
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE PIPE_Define (cdnaminp,cdnamout,
     $                        cdpinp,cdpout,kinfo)
C
      CHARACTER*8, cdnaminp,cdnamout,cdpinp,cdpout
      INTEGER kinfo
C
      cdnaminp=cdnamout
      cdpinp=cdpout
      kinfo=1
C
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE PIPE_Stepi (cdmodnam, kjm,
     $                       kistep, kifcpl, kidt, kinfos)
C
      INTEGER kistep, kifcpl, kidt, kinfos, kjm
      CHARACTER*6 cdmodnam
C
      cdmodnam=' '
      kistep=kifcpl
      kidt=kinfos
      kjm=1
C
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE PIPE_Recv (cdname, kiter)
C
      CHARACTER*8 cdname
      INTEGER kiter
C
      cdname=' '
      kiter=1
C
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE PIPE_Send (cdname, kiter)
      CHARACTER*8 cdname
      INTEGER kiter
C
      cdname=' '
      kiter=1
C
      RETURN
      END

