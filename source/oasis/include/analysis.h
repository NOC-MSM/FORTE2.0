C
C -- analysis.h   18-08-95   Version 2.0beta   Author: Laurent Terray
C    **********   01-02-96   Version 2.0 : addition of cdqdt,cgrdmap,nlumap
C                                          and nmapfl
C                 05-08-96   Version 2.1 : addition of nlusub, cgrdsub 
C                                          ctypsub and nsubfl (subgrid)
C                 29-08-96   Version 2.1 : new input for subgrid, mozaic
C                                          and anais, maximum number of
C                                          neighbors. New input for redglo
C                 14-04-99   Version 2.3 : new input for extrap called by
C                                          glored. Change periodicity 
C                                          variables.
C@
C@  Contents : variables related to the set of analysis for each field
C@  --------
C@
C@ Mask :
C@
C@ -- amskval : mask value
C@
C@ Mozaic :
C@
C@ -- cgrdmap : file name for grid mapping used in mozaic (1D)
C@
C@ -- nlumap  : logical units associated to previous files (1D)
C@
C@ -- nmapfl  : mapping dataset identificator number (1D)
C@
C@ -- nmapvoi : maximum number of neighbors (1D)
C@
C@ Invert :
C@
C@ -- cxordbf : field ordering (longitude) before interpolation (1D)
C@
C@ -- cyordbf : field ordering (latitude) before interpolation (1D)
C@
C@ Reverse :
C@
C@ -- cxordaf : field ordering (longitude) after interpolation (1D)
C@
C@ -- cyordaf : field ordering (latitude) after interpolation (1D)
C@
C@ Extrap :
C@
C@ -- cextmet : extrapolation method (1D)
C@
C@ -- neighbor : number of neighbors used in extrapolation (1D)
C@
C@ -- neighborg : number of neighbors used in extrapolation when extrap
C@                is called by GLORED (1D)
C@
C@ -- cgrdext : file names for data used in extrapolation WEIGHT (1D)
C@
C@ -- nluext  : logical units associated to previous files (1D)
C@
C@ -- nextfl  : extrapolation dataset identificator number (1D)
C@
C@ Interp :
C@
C@ -- cintmet : interpolation method (1D)
C@
C@ -- cgrdtyp : source grid type (1D)
C@
C@ -- csper   : source grid periodicity type (Periodic, Regional, Overlap)
C@
C@ -- ctper   : target grid periodicity type (Periodic, Regional, Overlap)
C@
C@ -- cfldtyp : field type (scalar or vector) (1D)
C@
C@ Filling :
C@
C@ -- cfilfic : file name for climatological field to complete model field (1D)
C@
C@ -- nlufil  : logical units connected to previous files (1D)
C@
C@ -- cficmet : filling method (1D)
C@
C@ -- cfldcor : field name for flux correction term due to SST filling
C@
C@ -- nlucor  : logical unit used to write flux correction term
C@
C@ Conserv :
C@
C@ -- cconmet : conservation method (1D)
C@
C@ Glored, redglo :
C@
C@ -- ntronca : gaussian troncature for reduced <-> global gaussian grid (1D)
C@
C@ -- cmskrd : extrapolation flag to handle processing reduced grid values (1D)
C@
C@ Correct :
C@
C@ -- afldcoef : main field multiplicative coefficient (1D)
C@
C@ -- ncofld : number of additional fields in correction formula (1D)
C@
C@ -- ccofic : file names for additional data files (2D)
C@
C@ -- nludat : logical units associated to previous files (2D)
C@
C@ -- ccofld : symbolic names for additional fields (2D)
C@
C@ -- acocoef : multiplicative coefficients for additional fields (2D)
C@
C@ Blasold :
C@
C@ -- afldcobo : main field multiplicative coefficient (1D)
C@
C@ -- nbofld : number of additional fields in linear combination formula (1D)
C@
C@ -- cbofld : symbolic names for additional fields (2D)
C@
C@ -- abocoef : multiplicative coefficients for additional fields (2D)
C@
C@ Blasnew :
C@
C@ -- afldcobn : main field multiplicative coefficient (1D)
C@
C@ -- nbnfld : number of additional fields in linear combination formula (1D)
C@
C@ -- cbnfld : symbolic names for additional fields (2D)
C@
C@ -- abncoef : multiplicative coefficients for additional fields (2D)
C@
C@ Subgrid :
C@
C@ -- cgrdsub : file name for subgrid data used in subgrid (1D)
C@
C@ -- nlusub  : logical units associated to previous files (1D)
C@
C@ -- ctypsub : type of subgrid interpolation (solar or non solar) (1D)
C@
C@ -- nsubfl  : subgrid dataset identificator number (1D)
C@
C@ -- nsubvoi : maximum number of neighbors (1D)
C@
C@ -- cfldcoa : coarse grid field name (1D)
C@
C@ -- cfldfin : fine grid field name (1D)
C@
C@ -- cdqdt   : first order taylor coefficient (1D)
C@
C     -------------------------------------------------------------------
C
      INTEGER neighbor(jpfield), ntronca(jpfield), ncofld(jpfield)
      INTEGER neighborg(jpfield)
      INTEGER nbofld(jpfield), nbnfld(jpfield)
      INTEGER nludat(jpcomb,jpfield), nlucor, nlufil(jpfield)
      INTEGER nlumap(jpfield), nmapfl(jpfield), nmapvoi(jpfield)
      INTEGER nlusub(jpfield), nsubfl(jpfield), nsubvoi(jpfield)
      INTEGER nluext(jpfield) , nextfl(jpfield)
      INTEGER nosper(jpfield), notper(jpfield)
C
      REAL amskval(jpfield)
      REAL acocoef(jpcomb,jpfield), abocoef(jpcomb,jpfield)
      REAL abncoef(jpcomb,jpfield)
      REAL afldcoef(jpfield), afldcobo(jpfield), afldcobn(jpfield)
C
      CHARACTER*8 cxordbf(jpfield), cyordbf(jpfield), cxordaf(jpfield)
      CHARACTER*8 cyordaf(jpfield), cextmet(jpfield), cintmet(jpfield)
      CHARACTER*8 cgrdtyp(jpfield), cfldtyp(jpfield), cfilfic(jpfield)
      CHARACTER*8 cfilmet(jpfield), cconmet(jpfield), cfldcoa(jpfield)
      CHARACTER*8 cfldfin(jpfield), ccofld(jpcomb,jpfield)
      CHARACTER*8 cbofld(jpcomb,jpfield), cbnfld(jpcomb,jpfield)
      CHARACTER*8 ccofic(jpcomb,jpfield), cfldcor, cdqdt(jpfield)
      CHARACTER*8 cgrdmap(jpfield), cmskrd(jpfield)
      CHARACTER*8 cgrdsub(jpfield), ctypsub(jpfield)
      CHARACTER*8 cgrdext(jpfield)
      CHARACTER*8 csper(jpfield), ctper(jpfield)
C
      COMMON / comian / neighbor, ntronca, ncofld, nbofld, nbnfld,
     $                  nludat, nlucor, nlufil, 
     $                  nlumap, nmapfl, nmapvoi,
     $                  nlusub, nsubfl, nsubvoi,
     $                  nluext, nextfl, neighborg,
     $                  nosper, notper
      COMMON / comran / amskval, acocoef, abocoef, abncoef,
     $                           afldcoef, afldcobo, afldcobn
      COMMON / comcan / cxordbf, cyordbf, cxordaf, cyordaf, cextmet,
     $                  cintmet, cgrdtyp, cfldtyp, cfilfic, cfilmet,
     $                  cconmet, ccofld, cbofld, cbnfld, cgrdmap,
     $                  cfldcoa, cfldfin, cdqdt, ccofic, cfldcor,
     $                  cgrdsub, ctypsub, cmskrd, cgrdext,
     $                  csper, ctper
C
C     -------------------------------------------------------------------
