CCC
CCC
      SUBROUTINE SIPC_Define (kindex, kinfo)
C
      INTEGER kinfo, kindex
C
      kinfo=1
C
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE SIPC_End(kmodel) 
C
      INTEGER kmodel
C
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE SIPC_Init (cdjobnam, cdmodnam, kmodel, kinfo)
C
      CHARACTER*6 cdmodnam
      CHARACTER*3 cdjobnam
      INTEGER kinfo,kmodel
C
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE SIPC_Stepi (cdmodnam, kmodel, kstep, 
     $                       kfcpl, kdt, kinfo)
C
      CHARACTER*6 cdmodnam
      INTEGER kmodel, kstep, kfcpl, kdt, kinfo 
C
      cdmodnam=' '
      kstep=kfcpl
      kdt=kinfo
C
      RETURN
      END
