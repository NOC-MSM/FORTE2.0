#ifdef hcomments 
c
c %Z% SCCS module: %M%  version: %I%
c     Creation date: %G%
c
c-----------------------------------------------------------------------
c    Include file for netcdf routines
c-----------------------------------------------------------------------
c
#endif


#define NC_MAX_SNAPS_PER_FILE 10000

c
c  define netcdf word lengths
c
# define     INT_TYPE      mnc_int
# ifdef REAL_8
#  define    REAL_TYPE     mnc_double
# else
#  define    REAL_TYPE     mnc_float
# endif

c define pointer to an array type
c This cannot be a character type since these pass lengths too.
c It must also be one byte long.
c Suggest (in order of preference): byte , integer*1
c
#define VOIDP              byte

      integer MAXDIMS           !Maximum number of dimensions a variable can have
      parameter(MAXDIMS = 4) 
      integer MAXNAMELEN        !Maximum length a dimension or variable name can be
      parameter(MAXNAMELEN = 32) 
      
c
      character NOUNITS,NOEDGES
      parameter(NOUNITS=' ',NOEDGES=' ')

      logical p_up,p_down
      parameter(p_up=.false.,p_down=.true.)

      logical iseven,noteven
      parameter(noteven=.false.,iseven=.true.)

      logical nofill,hasfill
      parameter(nofill=.false.,hasfill=.true.)
      
      logical nooff,hasoff
      parameter(nooff=.false.,hasoff=.true.)
      
      logical noscale,hasscale
      parameter(noscale=.false.,hasscale=.true.)
      
      
c     Data types :      
      integer mnc_float, mnc_double,mnc_int
      parameter(mnc_float=0, mnc_double=1,mnc_int=2)
c     Data lengths in bytes
      integer SIZEOF_MNC_FLOAT,SIZEOF_MNC_DOUBLE,SIZEOF_MNC_INT
      parameter(SIZEOF_MNC_FLOAT=4,SIZEOF_MNC_DOUBLE=8,SIZEOF_MNC_INT=4)

c     Data length
      integer mnc_unlimited
      parameter(mnc_unlimited=-1)

c     Fill value
      _MOMA_REAL MNC_FILLVALUE
      REAL*8 FILLVALUER8,R8ZERO
      REAL*4 FILLVALUER4,R4ZERO
      integer FILLVALUEI4,I4ZERO
      parameter(MNC_FILLVALUE = -99.999d0, FILLVALUER8 = -99.999d0,
     &     FILLVALUER4 = -99.999, FILLVALUEI4 = -99 
     &     ,I4ZERO=0,R4ZERO=0.0,R8ZERO=0.d0)
      
      
c     Buffer for storing data
      real*4 mnc_bufr4((km+1)*imt*jmt)
      real*8 mnc_bufr8((km+1)*imt*jmt)
      integer mnc_bufi4((km+1)*imt*jmt)
      VOIDP mnc_bufc1((km+1)*imt*jmt*8)
      equivalence(mnc_bufr4,mnc_bufr8,mnc_bufi4,mnc_bufc1)
      common / mnc_buffer / mnc_bufr8 

c     Buffer for passing dimension information:
      integer mnc_length        !vector length
      integer mnc_ifirst,mnc_ivlen !First index to take, number of points to take
      integer mnc_opstart       !Where to start the write for this dimension
      logical mnc_lwrite        !true when writing to file
      character mnc_axis_type   !i,j,k or l - depending in axis type - x,y,z,t
      integer mnc_dimid         !dimension id
      common /mnc_dim_com_i/ mnc_length,mnc_ifirst,mnc_ivlen
     &     ,mnc_dimid,mnc_opstart
      common /mnc_dim_com_c/ mnc_axis_type
      common /mnc_com_l/ mnc_lwrite
      
      
c Functions
      logical fn_kmt3d,fn_kmu3d,fn_kmt2d,fn_kmu2d,fn_true
      external fn_kmt3d,fn_kmu3d,fn_kmt2d,fn_kmu2d,fn_true
 
