C
C -- experiment.h   26-08-95   Version 2.0   Author: Laurent Terray
C    ************   14-12-97   Version 2.2   addition of info mode (lmodinf)
C                   14-03-99   Version 2.3   CHARACTER*4 cjobnam (S. Valcke)
C@
C@  Contents : variables related to the simulation being performed
C@  --------
C@
C@ -- cjobnam : experiment name
C@
C@ -- cmodnam : models name (1D)
C@
C@ -- nmodel : number of models being coupled
C@
C@ -- nmseq : number of sequential models
C@
C@ -- lmodinf : information mode (extended header)
C@
C     -------------------------------------------------------------------
C
      INTEGER nmodel, nmseq
C
      CHARACTER*4 cjobnam
      CHARACTER*6 cmodnam(jpmodel)
C
      LOGICAL lmodinf
C
      COMMON / comcex / cjobnam, cmodnam
      COMMON / comiex / nmodel, nmseq
      COMMON / comlex / lmodinf
C
C     -------------------------------------------------------------------




