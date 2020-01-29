      BLOCK DATA blkdata
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 0 *
C               * -------------     ------- *
C               *****************************
C
C**** *blkdata* - Block data routine
C
C     Purpose:
C     -------
C     Initialize some COMMON variables
C
C**   Interface:
C     ---------
C     None
C
C     Input:
C     -----
C     None
C
C     Output:
C     ------
C     None
C
C     Workspace:
C     ---------
C     None
C
C     Externals:
C     ---------
C     None
C
C     Reference:
C     ---------
C     See OASIS manual (1995)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.0beta   L. Terray      95/10/01  created
C       2.0       L. Terray      96/02/01  modified: addition of nitfn,
C                                          nddeb, disparition of ctranam
C       2.1       L. Terray      96/08/07  modified: addition of label
C                                          and new reduced grid 
C       2.2       L. Terray      97/12/14  added: default for info mode
C                                          and signal values
C       2.3       S. Valcke      99/03/16  added ECMWF T213 and T319
C       2.3       S. Valcke      99/03/26  changed troncature for number of 
C                                          latitude between equator and pole
C       2.3       S. Valcke      99/03/30  added definition of NINENN file 
C       2.3       L. Terray      99/08/02  cleaning old stuff
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'parameter.h'
      INCLUDE 'label.h'
      INCLUDE 'hardware.h'
      INCLUDE 'field.h'
      INCLUDE 'experiment.h'
      INCLUDE 'timestep.h'
      INCLUDE 'memory.h'
      INCLUDE 'calendar.h'
      INCLUDE 'smooth.h'
      INCLUDE 'gauss.h'
      INCLUDE 'anais.h'
      INCLUDE 'extrapol.h'
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C
C*    1. Default values
C        --------------
C
      DATA nfield/10/, nmodel/2/, nmseq/1/
      DATA cmodnam(1),cmodnam(2)/'atmos','ocean'/
      DATA ntime/432000/, niter/5/, nitfn/4/, nstep/86400/ 
      DATA ndate/00000000/, nddeb/00000000/
      DATA cmach/'CRAY'/, cchan/'PIPE'/, cjobnam/'TST '/
      DATA nsigfpe/8/, nsigcld/18/, nignore/0/, ncatch/-1/
      DATA lmodinf/.TRUE./
      DATA ntiret/5/, ntiogp/1200/, ntiout/300/
      DATA cglonsuf/'.lon'/, cglatsuf/'.lat'/, cmsksuf/'.msk'/,
     $    csursuf/'.srf'/
      DATA cgrdnam/'grids'/, cmsknam/'masks'/, csurnam/'areas'/
      DATA crednam/'maskr'/
      DATA cwanaisg/'gweights'/, cwanaism/'mweights'/,
     $     cnaisout/'anaisout'/
      DATA cwninenn/'nweights'/
      DATA nsltb/18/, nslte/26/, nnltb/49/, nnlte/41/, nliss/5/
      DATA nwlgmx/57/, nelgmx/229/
      DATA qalfa/0.125/, qbeta/0.25/
C
C* Memory allocation
C
      DATA memtot/jpmxold,jpmxnew/, nused/0,0/
C
C* Field label definition
C
      DATA cfldlab(1), cfldlab(2), cfldlab(3), cfldlab(4), cfldlab(5),
     $    cfldlab(6), cfldlab(7), cfldlab(8), cfldlab(9), cfldlab(10),
     $    cfldlab(11), cfldlab(12), cfldlab(13), cfldlab(14),
     $    cfldlab(15), cfldlab(16), cfldlab(17), cfldlab(18),
     $    cfldlab(19), cfldlab(20), cfldlab(21), cfldlab(22),
     $    cfldlab(23), cfldlab(24), cfldlab(25), cfldlab(26),
     $    cfldlab(27), cfldlab(28), cfldlab(29), cfldlab(30),
     $    cfldlab(31), cfldlab(32), cfldlab(33), cfldlab(34),
     $    cfldlab(35)/
     $' sea surface temperature      ',' sea-ice extent             ',
     $' zonal current stress         ',' meridional current stress  ',
     $' total heat flux              ',' non solar heat flux        ',
     $' solar heat flux              ',' latent heat flux           ',
     $' sensible heat flux           ',' longwave heat flux         ',
     $' direct solar heat flux       ',' diffuse solar heat flux    ',
     $' upward solar heat flux       ',' upward longwave heat flux  ',
     $' downward longwave heat flux  ',' emissivity                 ',
     $' surface albedo               ',' snow-covered fraction area ',
     $' lower level temperature      ',' lower level wind module    ',
     $' lower level geopotential     ',' lower level humidity       ',
     $' zonal wind stress            ',' meridional wind stress     ',
     $' evaporation                  ',' total precipitation        ',
     $' liquid precipitation         ',' solid precipitation        ',
     $' water flux (P + R - E)       ',' precipitation - evaporation',
     $' ice-covered fraction area    ',' river run off              ',
     $' surface pressure             ',' surface temperature        ',
     $' nonsolar heat flux derivative'/
C
C* Reduced gaussian grid data with no aliasing near the poles
C
C - T21
      DATA ninip16 /
     $   20,30,40,48,54,60,64,64,64,64,64,64,64,64,64,64/
C - T31
      DATA ninip24 /
     $    20,30,40,48,54,60,64,64,64,64,64,64,64,64,64,64,
     $    96,96,96,96,96,96,96,96/
C - T42
      DATA ninip32 /
     $    20,30,40,48,48,54,54,64,72,80,80,90,96,100,
     $    108,108,120,120,120,128,128,128,128,128,
     $    128,128,128,128,128,128,128,128/
C - T63
      DATA ninip48 /
     $    16, 16, 18, 24, 30, 36, 48, 48, 54, 60, 72, 72, 80, 90, 90
     $    ,96,100,108,120,120,128,128,144,144,144,144,150,160,160,160
     $    ,162,180,180,180,180,180,180,192,192,192,192,192,192,192,192
     $    ,192,192,192/
C - T106
      DATA ninip80 /
     $    16, 16, 18, 24, 30, 36, 48, 50, 60, 64, 72, 80, 80, 90, 96
     $   ,100,108,120,120,128,128,144,144, 144,150,160,162,180,180,180
     $   ,192,192,192,200,216,216,216,216, 240,240,240,240,240,250,250
     $   ,250,256,270,270,270,270,288,288, 288,288,288,288,300,300,300
     $   ,300,300,320,320,320,320,320,320, 320,320,320,320,320,320,320
     $   ,320,320,320,320,320/
C- T213- T319
      DATA ninip160 /
     $      18, 25, 36, 40, 45, 50, 60, 64, 72, 72, 80, 90, 90, 96,108
     $    ,120,120,125,128,135,144,150,160,160,180,180,180,192,192,200
     $    ,216,216,225,225,240,240,243,250,256,270,270,288,288,288,300
     $    ,300,320,320,320,320,324,360,360,360,360,360,360,375,375,375
     $    ,384,384,400,400,400,405,452,432,432,432,432,450,450,450,450
     $    ,480,480,480,480,480,480,480,500,500,500,500,500,512,512,540
     $    ,540,540,540,540,540,540,540,576,576,576,576,576,576,576,576
     $    ,576,576,600,600,600,600,600,600,600,600,600,640,640,640,640
     $    ,640,640,640,640,640,640,640,640,640,640,640,640,640,640,640
     $    ,640,640,640,640,640,640,640,640,640,640,640,640,640,640,640
     $    ,640,640,640,640,640,640,640,640,640,640/
C
      DATA nredu16/1784/, nredu24/3320/, nredu32/6232/
      DATA nredu48/12228/, nredu80/23444/, nredu160/138346/
C
      DATA amskred/9999999.0/
C
C*    2. End of routine
C        --------------
C
      END

