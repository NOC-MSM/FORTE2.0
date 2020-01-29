C
C -- string.h   26-07-95   Version 2.0   Author: Laurent Terray
C    *******    25-09-96   Version 2.1   Addition of extra time step (nfend)
C                                        and field integral flag (nintflx)
C               12-11-97   Version 2.2   Addition of nmxdel
C@
C@  Contents : set of self consistent strings for each field
C@  --------
C@
C@ First line of SSCS :
C@ ------------------
C@
C@ -- cnaminp : symbolic name of input fields
C@
C@ -- cnamout : symbolic name of output fields
C@
C@ -- numlab : label number of exchanged fields
C@
C@ -- nfexch : coupling frequency of exchanged fields (in days)
C@
C@ -- ntrans : number of analysis performed for each field
C@
C@ -- cficinp : file name for input field 
C@
C@ -- cficout : file name for output field 
C@
C@ -- nluninp : logical unit for input field
C@
C@ -- nlunout : logical unit for output field
C@
C@ -- cstate : field I/O status
C@
C@ Second line of SSCS :
C@ -------------------
C@
C@ -- nlonbf : number of longitudes for initial fields
C@
C@ -- nlatbf : number of latitudes for initial fields
C@
C@ -- nlonaf : number of longitudes for initial fields
C@
C@ -- nlataf : number of latitudes for initial fields
C@
C@ -- cficbf : root name of specific files for initial fields
C@
C@ -- cficaf : root name of specific files for final fields
C@
C@ -- nseqn  : model sequential index
C@
C@ -- nfinit : model initialization flag
C@
C@ -- nmxdel : nfinit array maximum element (not in namcouple)
C@
C@ -- nfend  : extra time step flag
C@
C@ -- nintflx : field integral flag
C@
C@ Third line of SSCS :
C@ ------------------
C@
C@ -- canal : names of analysis performed for each field 
C@
C     -------------------------------------------------------------------
C
      INTEGER numlab(jpfield), nfexch(jpfield), ntrans(jpfield)
      INTEGER nluinp(jpfield), nluout(jpfield)
      INTEGER nlonbf(jpfield), nlatbf(jpfield)
      INTEGER nlonaf(jpfield), nlataf(jpfield)
      INTEGER nseqn(jpfield), nfinit(jpfield),
     $        nfend(jpfield), nintflx(jpfield), nmxdel
C
      CHARACTER*8 cnaminp(jpfield), cnamout(jpfield),
     $            canal(jpanal,jpfield),
     $            cficinp(jpfield), cficout(jpfield)
      CHARACTER*8 cficbf(jpfield), cficaf(jpfield), cstate(jpfield)
C 
      COMMON / comcst / cnaminp, cnamout, canal, cficinp, cficout,
     $                  cficbf, cficaf, cstate
      COMMON / comist / numlab, nfexch, ntrans, nluinp, nluout,
     $                  nlonbf, nlatbf, nlonaf, nlataf,
     $                  nseqn, nfinit, nfend, nintflx, nmxdel
C
C     -------------------------------------------------------------------
