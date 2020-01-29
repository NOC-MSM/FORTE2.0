#ifdef hcomments 
c
c %Z% SCCS module: %M%  version: %I%
c     Creation date: %G%
c
c-----------------------------------------------------------------------
c    Include file for hdf routines
c-----------------------------------------------------------------------
c
#endif

c define maximum number of items to be found in one HDF file
#define MAXDATASETS	20

c define the label lengths for these values
#define MAXLABELS       15
#define MAXLABLEN       30
#define MAXFORMATLEN    20
#define MAXUNITLEN      12

#define MAXDIMS         5		
#define MAXFIDS         15 	

#define MODEL_1	        1
#define MODEL_2		2

#define OBSERVATIONAL	1
#define MODELLED	2

#define LEVITUS		1
#define RESTART		2
#define SNAPSHOT	3

#define RESOLUTION_4	1
#define RESOLUTION_6	2

#define NON_ROTATED	1
#define ROTATED		2

#define TGRID		1

#define FMODE_ATTR	'FMODE'
#define FTYPE_ATTR	'FTYPE'
#define TGRID_ATTR      'T_GRID'
#define LEVEL_ATTR      'LEVEL'
#define TSTEP_ATTR      'TIMESTEP'
#define MYEAR_ATTR      'YEAR'
#define MDAY_ATTR       'DAY'
#define MSEC_ATTR       'SECONDS'
#define RESOL_ATTR      'RESOLUTION'
#define DATE_ATTR       'DATE'
#define VERSION_ATTR    'VERSION'
#define ROTATION_ATTR   'ROTATION'

c
c  define hdf word lengths
c
# define INTEGER_HDF    DFNT_INT32
# ifdef REAL_8
#  define REAL_HDF      DFNT_FLOAT64
# else
#  define REAL_HDF      DFNT_FLOAT32
# endif
c
c  define hdf functions
c
      integer*4  sfcreate, sfstart, sfsdtstr, sfwdata,
     &           sfsdmname, sfsdmstr,sfdimid, sfrdata,
     &           sfendacc, sfsfill, sfsdscale, sfend, 
     &           sfsattr, sfn2index, sfselect
      external   sfcreate, sfstart, sfsdtstr, sfwdata,
     &           sfsdmname, sfsdmstr,sfdimid, sfrdata,
     &           sfendacc, sfsfill, sfsdscale, sfend, 
     &           sfsattr, sfn2index, sfselect
c
      integer narchv,num2d
      parameter (NUM2D = 3)
#ifdef w_write
      parameter(NARCHV = 8)
#else
      parameter(NARCHV = 7)
#endif

      integer rank2,rank3
c
c  define common blocks used by moma hdf routines
c
      integer*4 dims2,dims3
      character     labels2*MAXLABLEN, labels3*MAXLABLEN
      character    units2*MAXUNITLEN, units3*MAXUNITLEN
      character  fmats2*MAXFORMATLEN, fmats3*MAXFORMATLEN
      character   lonlabel*12,latlabel*12
      common/hdf2/rank2,dims2(MAXDIMS),labels2(MAXDIMS),
     &   units2(MAXDIMS),fmats2(MAXDIMS),lonlabel(2),latlabel(2)
      common/hdf3/rank3,dims3(MAXDIMS),labels3(MAXDIMS),
     &   units3(MAXDIMS),fmats3(MAXDIMS) 
c
      logical*4 around,vartgrd(NARCHV)
c
c23456789012345678901234567890123456789012345678901234567890123456789012
      character hdffile*80,cfext*10,varname*25,varunit*12,
     &          varid*25,varcordsys*12
      character tlabel*MAXLABLEN

      common /hdfl/ around,vartgrd
      integer*4 itdims(MAXDIMS),itsize
      common /hdfi/ itdims,itsize
      
      common /hdfa/ hdffile,cfext(2),tlabel(MAXDIMS)
     &                ,varname(NARCHV),varunit(NARCHV)
     &                ,varid(NARCHV),varcordsys(NARCHV)
      
