C
C -- unit.h   23-08-95   Version 2.0   Author: Laurent Terray
C    ******   20-06-97   Version 2.2   Mods: add nudum (S. Valcke)
C             30-03-99   Version 2.3   Mods: add nulgn (S. Valcke)
C@
C@  Contents : unit numbers
C@  --------
C@
C@ -- nulin : logical unit for coupler input
C@
C@ -- nulou : logical unit for coupler output
C@
C@ -- nulgr : logical unit for gcm's grids
C@
C@ -- nulma : logical unit for gcm's masks
C@
C@ -- nulsa : logical unit for gcm's surfaces
C@
C@ -- nultr : trace file for CLIM and PVM messages
C@
C@ -- nulcc : file for ANAISM data
C@
C@ -- nulgg : file for ANAISG data
C@
C@ -- nulgn : file for NINENN weights and addresses
C@
C@ -- nulan : file for ANAIS(M-G) specific output
C@
C@ -- nulrd : file for reduced grid masks
C@
C@ -- nudum : dummy file signaling that SVIPC pools for exchange of 
C@            initial infos are opened
C@
C     -------------------------------------------------------------------   
C
      INTEGER  nulin, nulou, nulgr, nulma, nulsu, nultr, nulcc,
     $         nulgg, nulan, nulrd, nudum, nulgn 
C
      COMMON / comlun / nulin, nulou, nulgr, nulma, nulsu, nultr,
     $                  nulcc, nulgg, nulan, nulrd, nudum, nulgn
C
C     -------------------------------------------------------------------   

