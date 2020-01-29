      SUBROUTINE fiasco (poutp, ptlon, ptlat, ptsurf,
     $                   ktmsk, ktlon, ktlat, cdtper, ktper,
     $                   pinpt, pslon, pslat, pssurf,
     $                   ksmsk, kslon, kslat, cdsper, ksper,
     $                   plonz, platz, psgrb, psgra, psfrb, psfra,
     $                   kfield, cdint, cdtyp, cddim)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *fiasco* - Interpolation driver
C
C     Purpose:
C     -------
C     Interpolation from source grid to target grid
C     Use Fast Scalar Interpolator (FSCINT) package from Yves Chartier
C     or ANAIS software from Olivier Thual et al.
C
C**   Interface:
C     ---------
C       *CALL*  *fiasco* (poutp, ptlon, ptlat, ptsurf,
C                         ktmsk, ktlon, ktlat, cdtper, ktper,
C                         pinpt, pslon, pslat, pssurf,
C                         ksmsk, kslon, kslat, cdsper, ksper,
C                         plonz, platz, psgrb, psgra, psfrb, psfra,
C                         cdint, cdtyp, cddim)*
C
C     Input:
C     -----
C                ktlon  : number of longitudes for target grid
C                ktlat  : number of latitudes for target grid
C                ptlon  : longitudes of target grid (real 2D)
C                ptlat  : latitudes of target grid (real 2D)
C                ktmsk  : mask of target grid (integer 2D)
C                ptsurf : target grid square surfaces (real 2D)
C                cdtper : target grid periodicity
C                ktper  : number of overlapped points for target grid
C                pinpt  : input field on source grid (real 2D)
C                kslon  : number of longitudes for source grid
C                kslat  : number of latitudes for source grid
C                pslon  : longitudes of source grid (real 2D)
C                pslat  : latitudes of source grid (real 2D)
C                ksmsk  : mask of source grid (integer 2D)
C                pssurf : source grid square surfaces (real 2D)
C                cdsper : source grid periodicity
C                ksper  : number of overlapped points for source grid
C                kfield : current field number
C                cdint  : type of interpolation to be performed
C                cdtyp  : type of source grid
C                cddim  : type of field (scalar or vector)
C
C     Output:
C     ------
C                poutp : output field on target grid (real 2D)
C
C     Workspace:
C     ---------
C     These are work arrays passed as arguments :
C     plonz, platz, psgrb, psgra, psfrb, psfra 
C
C     Externals:
C     ---------
C     rgoptc, rgoptr, namset, nagset, cxgaig, igscint, namsst,
C     nagsst, naflux.
C
C     Reference:
C     ---------
C     See OASIS manual (1995)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       1.0       L. Terray      94/01/01  created
C       1.1       L. Terray      94/08/01  modified: interpolation differ
C                                          for scalar or vector in fscint
C       2.0       L. Terray      95/12/15  modified: new structure
C       2.2       L. Terray      97/11/28  added: grid type A,B,L in fscint
C       2.3       L. Terray      99/03/01  corrected: calls to na(g-m)sst with
C                                          inclusion of pointer ipdeb
C       2.3       S. Valcke      99/04/30  added: printing levels
C       2.3       L. Terray      99/08/11  modified: fscint extrapolation
C                                          for periodic Z grids
C       2.3       L. Terray      99/09/15  changed periodicity variables
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'parameter.h'
      INCLUDE 'unit.h'
      INCLUDE 'anais.h'
      INCLUDE 'timestep.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C
      REAL poutp(ktlon,ktlat), ptlon(ktlon,ktlat), ptlat(ktlon,ktlat)
      REAL pinpt(kslon,kslat), pslon(kslon,kslat), pslat(kslon,kslat)
      REAL pssurf(kslon,kslat), ptsurf(ktlon,ktlat)
      REAL plonz(ktlon), platz(ktlat), psgrb(kslon,kslat),
     $     psgra(ktlon,ktlat), psfrb(kslon,kslat), psfra(ktlon,ktlat)
      INTEGER ktmsk(ktlon,ktlat), ksmsk(kslon,kslat)
      CHARACTER*8 cdsper, cdtper
C
C* ---------------------------- Local declarations ----------------------
C
      CHARACTER*1 cdtyp, clref, cltyp
      CHARACTER*6 cddim, cldim
      CHARACTER*8 cdint, clord, clflg, clind
      LOGICAL llinit, llsym
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE fiasco  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Interpolation package driving routine'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* Assign local value to grid and field type variables to deal 
C  with FSCINT set-up
C
      cltyp = cdtyp
      cldim = cddim
C
C* set up interpolation method
C
      IF (cdint .EQ. 'BILINEAR') THEN
          clflg = 'FSCINT'
          CALL rgoptc('INTERP', 'LINEAIR', .TRUE.)
        ELSEIF (cdint .EQ. 'BICUBIC') THEN
          clflg = 'FSCINT'
          CALL rgoptc('INTERP', 'CUBIQUE', .TRUE.)
        ELSEIF (cdint .eq. 'NNEIBOR') THEN
          clflg = 'FSCINT'
          CALL rgoptc('INTERP', 'VOISIN', .TRUE.)
        ELSEIF (cdint .EQ. 'SURFMESH') THEN
          clflg = 'ANAISM'
          llinit = linit(kfield)
        ELSEIF (cdint .EQ. 'GAUSSIAN') THEN
          clflg = 'ANAISG'
          llinit = linit(kfield)
        ELSE
          WRITE (UNIT = nulou,FMT = *) '         ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $           ' ===>>>> : unknown kind of interpolator'
          WRITE (UNIT = nulou,FMT = *) 
     $           ' =======                   ============'
          WRITE (UNIT = nulou,FMT = *) 
     $           '                     --->>>  CDINT = ',cdint
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $        ' We STOP!!! check variable syntax '
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL HALTE ('STOP in fiasco')
      ENDIF
      IF (clflg .eq. 'FSCINT') THEN
          CALL rgoptc('INTERP', clord, .FALSE.)
        ELSEIF (clflg .EQ. 'ANAISM') THEN
          clord = 'SURFMESH'
        ELSEIF (clflg .EQ. 'ANAISG') THEN
          clord = 'GAUSSIAN'
        ELSE
          clord ='UNKNOWN'
      ENDIF
      IF (nlogprt .GE. 2) THEN
          CALL prcout
     $    ('Current interpolator is clord = ', clord, 1)
      ENDIF
C
C* set up value to be given in case of extrapolation in fscint
C
      IF (clflg .eq. 'FSCINT') THEN
          IF (cltyp .EQ. 'Z') THEN
C
C* No extrapolation for Z grids (global grid)
C
              CALL rgoptc ('EXTRAP', 'OUI', .TRUE.)
          ELSE 
              CALL rgoptc ('EXTRAP', 'VOISIN', .TRUE.)
          ENDIF 
          CALL rgoptc ('EXTRAP', clind , .false.)
          IF (nlogprt .GE. 2) THEN
              CALL prcout
     $        ('Current extrapolation in FSCINT is clind =', clind, 1)
          ENDIF
      ENDIF
C
C* set up grid descriptors for fscint interpolator
C
      IF (clflg .eq. 'FSCINT') THEN
          IF (cltyp .EQ. 'Z' .OR. cltyp .EQ. 'Y') THEN
C
C* here ig* descriptors are arbitrary (date of louis XVI's death)
C
              ig1 = 21
              ig2 = 0
              ig3 = 1
              ig4 = 1793
              zdllg = 1.0
              zdllt = 1.0
              zlgin = 0.0
              zltin = 0.0
              clref = 'L'
              CALL cxgaig (clref, ig1z, ig2z, ig3z, ig4z,
     $                     zltin, zlgin, zdllt, zdllg)
              DO 110 ji = 1, kslon
                plonz(ji) = pslon(ji,1) + 1.0
 110          CONTINUE
              DO 120 jj = 1, kslat
                platz(jj) = pslat(1,jj) + 1.0
 120          CONTINUE
            ELSE IF (cltyp .EQ. 'G'.or. cltyp .EQ. 'A'
     $            .OR. cltyp .EQ. 'B') THEN
              ig1 = 0
              ig2 = 0
              ig3 = 0
              ig4 = 0
              llsym = .TRUE.
            ELSE IF (cltyp .EQ. 'L') THEN 
              zdllt = pslat(1,2) - pslat(1,1)
              zdllg = pslon(2,1) - pslon(1,1)
              zlgin = pslon(1,1)
              zltin = pslat(1,1)
              CALL cxgaig (cltyp, ig1, ig2, ig3, ig4,
     $            zltin, zlgin, zdllt, zdllg)
              llsym = .TRUE. 
            ELSE
              WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulou,FMT = *) 
     $               ' ===>>> : grid type not implemented yet'
              WRITE (UNIT = nulou,FMT = *) 
     $               ' ======   ====          ===========    '
              WRITE (UNIT = nulou,FMT = *) 
     $            '                --->>>  cltyp = ',
     $                        cltyp
              WRITE (UNIT = nulou,FMT = *) ' '
              WRITE (UNIT = nulou,FMT = *) 
     $             ' We STOP    !!! Check source grid type'
              WRITE (UNIT = nulou,FMT = *) ' '
              CALL HALTE ('STOP in fiasco')
          ENDIF
        ELSEIF (clflg .eq. 'ANAISM') THEN
          IF (llinit) THEN
C
C* Mask values for both models
C
              ismsq = 1
              itmsq = 1
C
C* If no intersection, give value imesh to nmesh array element
C
              imesh = 0
              iwlun = nulcc
C
C* Get pointers for ANAISM interpolator parameters
C
              ipdeb = (naismfl(kfield)-1)*jpwoa*jpgrd+1
              isdeb = (naismfl(kfield)-1)*jpgrd+1
C
C* Set up surfmesh interpolation
C
              CALL namset (pslon, pslat, ksmsk, ismsq, kslon, kslat,
     $                     cdsper, ksper,
     $                     ptlon, ptlat, ktmsk, itmsq, ktlon, ktlat,
     $                     cdtper, ktper,
C
C* Specific ANAISM parameters
C
     $                     amint(ipdeb), 
     $                     nmint(ipdeb),
     $                     naismvoi(kfield), psgrb, psgra,
     $                     niwtm(kfield), iwlun, imesh,
     $                     nmesh(isdeb), 
     $                     naismfl(kfield))
C
C* Switch initialization flag
C
              linit(kfield) = .FALSE. 
          ENDIF
        ELSEIF (clflg .eq. 'ANAISG') THEN
          IF (llinit) THEN
              ismsq = 1
              itmsq = 1
              iwlun = nulgg
C
C* Get pointers for Anaisg interpolator parameters
C
              ipdeb = (naisgfl(kfield)-1)*jpnoa*jpgrd+1
              CALL nagset (pslon, pslat, pssurf, 
     $                     ksmsk, ismsq, kslon, kslat,
     $                     ptlon, ptlat, ptsurf, 
     $                     ktmsk, itmsq, ktlon, ktlat,
C
C* Specific Anaisg parameters
C
     $                     agint(ipdeb),
     $                     ngint(ipdeb),
     $                     naisgvoi(kfield), psfrb, psfra,
     $                     varmul(kfield), niwtg(kfield), iwlun, 
     $                     naisgfl(kfield))
C
C* Switch off initialization flag
C
              linit(kfield) = .FALSE.  
          ENDIF
        ELSE
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $           ' ===>>> : unknown interpolation package'
          WRITE (UNIT = nulou,FMT = *) 
     $           ' ======   =======               ======='
          WRITE (UNIT = nulou,FMT = *) 
     $        '                    --->>>  clflg = ',
     $                        clflg
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $           ' We STOP        !!! Check interpolation package'
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL HALTE ('STOP in fiasco')
      ENDIF
C
C
C*    2. Interpolation
C        -------------
C
      IF (clflg .eq. 'FSCINT') THEN
          IF (cltyp .eq. 'Z' .OR. cltyp .EQ. 'Y') THEN
              CALL igscint (poutp, ktlon, ktlat, ptlat, ptlon,
     $                      pinpt, kslon, kslat, cltyp, clref,
     $                      ig1z, ig2z, ig3z, ig4z, .true.,
     $                      plonz, platz, cldim)
            ELSEIF (cltyp .eq. 'G'.or. cltyp .eq. 'A'
     $            .OR. cltyp .eq. 'B'.or. cltyp .eq. 'L') THEN
              CALL rgscint (poutp, ktlon, ktlat, ptlat, ptlon,
     $                      pinpt, kslon, kslat, cltyp,
     $                      ig1, ig2, ig3, ig4, llsym, cldim)
            ELSE 
              WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulou,FMT = *) 
     $               ' ===>>> grid type not implemented yet'
              WRITE (UNIT = nulou,FMT = *) 
     $               ' ====== ====          ===========    '
              WRITE (UNIT = nulou,FMT = *) 
     $            '                --->>>  cltyp = ',
     $                        cltyp
              WRITE (UNIT = nulou,FMT = *) ' '
              WRITE (UNIT = nulou,FMT = *) 
     $             ' WE STOP    !!! check source grid type'
              WRITE (UNIT = nulou,FMT = *) ' '
              CALL HALTE ('STOP in fiasco')         
          ENDIF
        ELSEIF (clflg .eq. 'ANAISM') THEN
          itmsq = 1
          ipdeb = (naismfl(kfield)-1)*jpwoa*jpgrd+1
          CALL namsst (poutp, ktmsk, itmsq, ktlon, ktlat,
     $                 amint(ipdeb), nmint(ipdeb), naismvoi(kfield),
     $                 pinpt, kslon, kslat)
        ELSEIF (clflg .eq. 'ANAISG') THEN
          itmsq = 1
          ipdeb = (naisgfl(kfield)-1)*jpnoa*jpgrd+1
          CALL nagsst (poutp, ktmsk, itmsq, ktlon, ktlat,
     $                 agint(ipdeb), ngint(ipdeb), naisgvoi(kfield),
     $                 pinpt, kslon, kslat)
        ELSE
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $           ' ===>>> : unknown interpolation package'
          WRITE (UNIT = nulou,FMT = *) 
     $           ' ======   =======               ======='
          WRITE (UNIT = nulou,FMT = *) 
     $           '                      --->>>  clflg = ',
     $                        clflg
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $           ' We STOP        !!! Check interpolation package'
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL HALTE ('STOP in fiasco')  
      ENDIF
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine fiasco ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
