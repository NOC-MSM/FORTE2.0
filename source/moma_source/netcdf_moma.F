#include "cpp_options.h"
#ifdef netcdf

      subroutine archwr_netcdf
      implicit none
#include "param.h"
#include "ctmngr.h"
#include "netcdf_moma.h"
#include "slabs.h"
#include "timelv.h"
#include "frees.h"
#include "levind.h"

c     local:
      character path*150
      logical around
c
c     This small common block stores the filename and time level index
c     to be used when many snap shots are written to a single file...
      character netcdffile*200, netcdffile2*200,number*12

      integer precision
      parameter(precision = REAL_TYPE)
      integer i
      logical clobber
      parameter(clobber=.false.) !Whether old archives can be over-written
c functions:
      integer lnblnk
      external lnblnk
c     Create filename
      write(number, '(i10.10)') nint(totday)
      path='./'    
      netcdffile = path(1:lnblnk(path))//'d'//
     &     number(3:10)//'.nc'
      inquire(file = netcdffile, exist = around )
      if(around.and.(.not.clobber)) then
c New file name is the name but with an additional letter added:
        i=0
        do while(around.and.(i.le.26))
          netcdffile2 =  path(1:lnblnk(path))//'d'//
     &     number(3:10)//'.'//achar(i+ichar('a'))//'.nc'
          inquire(file = netcdffile2, exist = around )
          i=i+1
        enddo
        if(i.gt.26) then
          write(*,*) 'ERROR could not create a new netcdf archive '
     &         ,'file : tried "',netcdffile(1:lnblnk(netcdffile)),
     &         'and "',path(1:lnblnk(path))//'d'//
     &     number(3:10)//'.[a-z].nc"'
          write(*,*) ' All files tried appear to exist already'
          STOP 'ERROR'
        endif
        netcdffile = netcdffile2
      endif
c     Create new file
      call new_netcdf_file(netcdffile)

      call cgs_to_base(km,t(1,1,1,1,np),0)
      call mnc_write_4d( netcdffile , t(1,1,1,1,np) ,
     &     'theta0','DEPTH','LONGITUDE_T','LATITUDE_T',' ',
     &     'sea_water_potential_temperature',
     &     'K',REAL_TYPE,precision,fn_kmt3d)
      call base_to_cgs(km,t(1,1,1,1,np),0)

      call cgs_to_base(km,t(1,1,1,2,np),2)
      call mnc_write_4d( netcdffile , t(1,1,1,2,np) ,
     &     'so','DEPTH','LONGITUDE_T','LATITUDE_T',' ',
     &     'sea_water_salinity',
     &     '1e-3',REAL_TYPE,precision,fn_kmt3d)
      call base_to_cgs(km,t(1,1,1,2,np),2)
c     Add special attributes for salinity
c      call add_scale_off(netcdffile, 'so', 1000.d0,35.d0,
c     &       REAL_TYPE)

      call cgs_to_base(km,u(1,1,1,np),1)
      call mnc_write_4d( netcdffile , u(1,1,1,np) ,
     &     'uo','DEPTH','LONGITUDE_U','LATITUDE_U',' ',
     &     'eastward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu3d)
      call base_to_cgs(km,u(1,1,1,np),1)

      call cgs_to_base(km,v(1,1,1,np),1)
      call mnc_write_4d( netcdffile , v(1,1,1,np) ,
     &     'vo','DEPTH','LONGITUDE_U','LATITUDE_U',' ',
     &     'northward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu3d)
      call base_to_cgs(km,v(1,1,1,np),1)

      call cgs_to_base(1,u0(1,1,np0),1)
      call mnc_write_4d( netcdffile , u0(1,1,np0) ,
     &     'buo','LONGITUDE_U','LATITUDE_U',' ',' ',
     &     'barotropic_eastward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu2d)
      call base_to_cgs(1,u0(1,1,np0),1)

      call cgs_to_base(1,v0(1,1,np0),1)
      call mnc_write_4d( netcdffile , v0(1,1,np0) ,
     &     'bvo','LONGITUDE_U','LATITUDE_U',' ',' ',
     &     'barotropic_northward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu2d)
      call base_to_cgs(1,v0(1,1,np0),1)

      call cgs_to_base(1,h0(1,1,np0),1)
      call mnc_write_4d( netcdffile , h0(1,1,np0) ,
     &     'zos','LONGITUDE_T','LATITUDE_T',' ',' ',
     &     'sea_surface_height_above_geoid',
     &     'm',REAL_TYPE,precision,fn_kmt2d)
      call base_to_cgs(1,h0(1,1,np0),1)

      call mnc_write_4d( netcdffile , kmt(1,1) ,
     &     'kmt','LONGITUDE_T','LATITUDE_T',' ',' ',
     &     'tracer_point_mask',
     &     'Max. Valid. Model Level',MNC_INT,MNC_INT,fn_true)

      call mnc_write_4d( netcdffile , kmu(1,1) ,
     &     'kmu','LONGITUDE_U','LATITUDE_U',' ',' ',
     &     'vector_point_mask',
     &     'Max. Valid. Model Level',MNC_INT,MNC_INT,fn_true)

      call mnc_write_4d( netcdffile , totsec ,
     &     'SECONDS',' ',' ',' ',' ',
     &     'Elapsed model time',
     &     'seconds',REAL_TYPE,mnc_double,fn_true)

      call mnc_write_4d( netcdffile , years ,
     &     'YEARS',' ',' ',' ',' ',
     &     'Elapsed model time',
     &     'years',REAL_TYPE,mnc_double,fn_true)

      call mnc_write_4d( netcdffile , totday ,
     &     'DAY',' ',' ',' ',' ',
     &     'Elapsed model time',
     &     'days',REAL_TYPE,mnc_double,fn_true)
      end                       !subroutine archwr_netcdf

c
c -------------------------------------------------------------------
c
      subroutine archrd_netcdf
      implicit none
#include "param.h"
#include "ctmngr.h"
#include "netcdf_moma.h"
#include "slabs.h"
#include "timelv.h"
#include "frees.h"
#include "levind.h"
#include "iounit.h"

c     local:
c     Whether any of the read fields mismatch the mask :
      logical lerr
      integer i,j,k
c
      character netcdffile*200

      integer precision

      lerr = .false.
      netcdffile=fnrest

      call mnc_read_4d( netcdffile , kmt(1,1) ,
     &     'kmt','LONGITUDE_T','LATITUDE_T',' ',' ',
     &     'tracer_point_mask',
     &     'Max. Valid. Model Level',MNC_INT,precision,fn_true)
      call boundarieskm(kmt)    !Fill halo points

      call mnc_read_4d( netcdffile , kmu(1,1) ,
     &     'kmu','LONGITUDE_U','LATITUDE_U',' ',' ',
     &     'vector_point_mask',
     &     'Max. Valid. Model Level',MNC_INT,precision,fn_true)
      call boundarieskm(kmu)    !Fill halo points

      call mnc_read_4d( netcdffile , t(1,1,1,1,np) ,
     &     'theta0','DEPTH','LONGITUDE_T','LATITUDE_T',' ',
     &     'sea_water_potential_temperature',
     &     'K',REAL_TYPE,precision,fn_kmt3d)
      call base_to_cgs(km,t(1,1,1,1,np),0)
      call boundaries(km,t(1,1,1,1,np)) !Fill halo points
      call check_field(km,t(1,1,1,1,np),kmt,.true.,.true.,lerr)

      call mnc_read_4d( netcdffile , t(1,1,1,2,np) ,
     &     'so','DEPTH','LONGITUDE_T','LATITUDE_T',' ',
     &     'sea_water_salinity',
     &     '1e-3',REAL_TYPE,precision,fn_kmt3d)
      call base_to_cgs(km,t(1,1,1,2,np),2)
      call boundaries(km,t(1,1,1,2,np)) !Fill halo points
      call check_field(km,t(1,1,1,2,np),kmt,.true.,.true.,lerr)

      call mnc_read_4d( netcdffile , u(1,1,1,np) ,
     &     'uo','DEPTH','LONGITUDE_U','LATITUDE_U',' ',
     &     'eastward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu3d)
      call base_to_cgs(km,u(1,1,1,np),1)
      call boundaries(km,u(1,1,1,np)) !Fill halo points
      call check_field(km,u(1,1,1,np),kmu,.true.,.false.,lerr)

      call mnc_read_4d( netcdffile , v(1,1,1,np) ,
     &     'vo','DEPTH','LONGITUDE_U','LATITUDE_U',' ',
     &     'northward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu3d)
      call base_to_cgs(km,v(1,1,1,np),1)
      call boundaries(km,v(1,1,1,np)) !Fill halo points
      call check_field(km,v(1,1,1,np),kmu,.true.,.true.,lerr)

      call mnc_read_4d( netcdffile , u0(1,1,np0) ,
     &     'buo','LONGITUDE_U','LATITUDE_U',' ',' ',
     &     'barotropic_eastward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu2d)
      call base_to_cgs(1,u0(1,1,np0),1)
      call boundaries(1,u0(1,1,np0)) !Fill halo points
      call check_field(1,u0(1,1,np0),kmu,.true.,.true.,lerr)

      call mnc_read_4d( netcdffile , v0(1,1,np0) ,
     &     'bvo','LONGITUDE_U','LATITUDE_U',' ',' ',
     &     'barotropic_northward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu2d)
      call base_to_cgs(1,v0(1,1,np0),1)
      call boundaries(1,v0(1,1,np0)) !Fill halo points
      call check_field(1,v0(1,1,np0),kmu,.true.,.true.,lerr)

      call mnc_read_4d( netcdffile , h0(1,1,np0) ,
     &     'zos','LONGITUDE_T','LATITUDE_T',' ',' ',
     &     'sea_surface_height_above_geoid',
     &     'm',REAL_TYPE,precision,fn_kmt2d)
      call base_to_cgs(1,h0(1,1,np0),1)
      call boundaries(1,h0(1,1,np0)) !Fill halo points
      call check_field(1,h0(1,1,np0),kmt,.true.,.true.,lerr)

      call mnc_read_4d( netcdffile , totsec ,
     &     'SECONDS',' ',' ',' ',' ',
     &     'Elapsed model time',
     &     'seconds',REAL_TYPE,mnc_double,fn_true)

      call mnc_read_4d( netcdffile , years ,
     &     'YEARS',' ',' ',' ',' ',
     &     'Elapsed model time',
     &     'years',REAL_TYPE,mnc_double,fn_true)

      call mnc_read_4d( netcdffile , totday ,
     &     'DAY',' ',' ',' ',' ',
     &     'Elapsed model time',
     &     'days',REAL_TYPE,mnc_double,fn_true)

c      if(lerr) STOP 'MISMATCHED DATA - If your data is correct,'//
c     &  ' edit netcdf_moma.F to prevent stopping here.'

c Copy this time level to all others
      do i=1,imt
       do j=1,jmt
          h0(i,j,nm0) = h0(i,j,np0)
          h0(i,j,nc0) = h0(i,j,np0)
          u0(i,j,nm0) = u0(i,j,np0)
          u0(i,j,nc0) = u0(i,j,np0)
          v0(i,j,nm0) = v0(i,j,np0)
          v0(i,j,nc0) = v0(i,j,np0)
          do k=1,km
            t(k,i,j,1,nc) = t(k,i,j,1,np)
            t(k,i,j,1,nm) = t(k,i,j,1,np)
            t(k,i,j,2,nc) = t(k,i,j,2,np)
            t(k,i,j,2,nm) = t(k,i,j,2,np)

            u(k,i,j,nc) = u(k,i,j,np)
            u(k,i,j,nm) = u(k,i,j,np)
            v(k,i,j,nc) = v(k,i,j,np)
            v(k,i,j,nm) = v(k,i,j,np)

          enddo
       enddo
      enddo


      end

c
c -------------------------------------------------------------------
c
      subroutine snap_netcdf
      implicit none
#include "param.h"
#include "ctmngr.h"
#include "netcdf_moma.h"
#include "slabs.h"
#include "timelv.h"
#include "frees.h"
#ifdef flux_write
# include "cvbc.h"
#endif
c     local:
      character path*150
      logical around
c
c     This small common block stores the filename and time level index
c     to be used when many snap shots are written to a single file...
      integer idft_snap
      character netcdffile*200
      common / com_netcdf_snap_i / idft_snap
      common / com_netcdf_snap_c / netcdffile

      integer precision
      parameter(precision = mnc_float)
#ifdef flux_write
      _MOMA_REAL SCALE_STF1,SCALE_STF2
      parameter(SCALE_STF1 = 4.184d4)
c     cal/cm^2/sec to W/m^2 into ocean =(cal/sec per Watt)*(cm^2 per m^2)
      parameter(SCALE_STF2 = -9.0102857143d6)
c     ppt/cm^2/sec to m/year of fresh water into ocean (P-E)
c      Assuming 1year=365.0 days & salinity of ocean = 35ppt.
c      Scale = (1000/35 ) * 86400 * 365 /100
c       = (1/salinity of ocean) * (secs per day) * (days per year) / (cm per m)
#endif

c functions:
      integer lnblnk
      external lnblnk
c Data...
      data idft_snap / 0 /

      idft_snap = idft_snap+1
      if(idft_snap.gt.NC_MAX_SNAPS_PER_FILE) then
       idft_snap = 1
      endif

c     First time ? Then change the filename
      if(idft_snap.eq.1) then
       write(netcdffile, '(i10.10)') nint(totday)
       path='./'                !No Path
       netcdffile = path(1:lnblnk(path))//'d'//
     &      netcdffile(3:10)//'.snap.nc'
       inquire(file = netcdffile, exist = around )
       if(around) then
        write(stdout,*)'Warning - Clobbering old snapshot file!'
       endif
c Kill old file and create new one
       call new_netcdf_file(netcdffile)
      endif

      call cgs_to_base(km,t(1,1,1,1,np),0)
      call mnc_write_4d( netcdffile , t(1,1,1,1,np) ,
     &     'theta0','DEPTH','LONGITUDE_T','LATITUDE_T','DAY',
     &     'sea_water_potential_temperature',
     &     'K',REAL_TYPE,precision,fn_kmt3d)
      call base_to_cgs(km,t(1,1,1,1,np),0)

      call cgs_to_base(km,t(1,1,1,2,np),2)
      call mnc_write_4d( netcdffile , t(1,1,1,2,np) ,
     &     'so','DEPTH','LONGITUDE_T','LATITUDE_T','DAY',
     &     'sea_water_salinity',
     &     '1e-3',REAL_TYPE,precision,fn_kmt3d)
      if(idft_snap.eq.1) then
      call base_to_cgs(km,t(1,1,1,2,np),2)
c     Add special attributes for salinity if this is the first write.
c        call add_scale_off(netcdffile, 'so', 1000.d0,35.d0,
c     &       REAL_TYPE)
      endif

      call cgs_to_base(km,u(1,1,1,np),1)
      call mnc_write_4d( netcdffile , u(1,1,1,np) ,
     &     'uo','DEPTH','LONGITUDE_U','LATITUDE_U','DAY',
     &     'eastward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu3d)
      call base_to_cgs(km,u(1,1,1,np),1)

      call cgs_to_base(km,v(1,1,1,np),1)
      call mnc_write_4d( netcdffile , v(1,1,1,np) ,
     &     'vo','DEPTH','LONGITUDE_U','LATITUDE_U','DAY',
     &     'northward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu3d)
      call base_to_cgs(km,v(1,1,1,np),1)

#ifdef w_write
      call cgs_to_base(kmp1,w(0,1,1),1)
      call mnc_write_4d( netcdffile , w(0,1,1) ,
     &     'wo','DEPTH_EDGES','LONGITUDE_T','LATITUDE_T','DAY',
     &     'upward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmt3d)
      call base_to_cgs(kmp1,w(0,1,1),1)
#endif

      call cgs_to_base(1,u0(1,1,np0),1)
      call mnc_write_4d( netcdffile , u0(1,1,np0) ,
     &     'buo','LONGITUDE_U','LATITUDE_U','DAY',' ',
     &     'barotropic_eastward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu2d)
      call base_to_cgs(1,u0(1,1,np0),1)

      call cgs_to_base(1,v0(1,1,np0),1)
      call mnc_write_4d( netcdffile , v0(1,1,np0) ,
     &     'bvo','LONGITUDE_U','LATITUDE_U','DAY',' ',
     &     'barotropic_northward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu2d)
      call base_to_cgs(1,v0(1,1,np0),1)

      call cgs_to_base(1,h0(1,1,np0),1)
      call mnc_write_4d( netcdffile , h0(1,1,np0) ,
     &     'zos','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'sea_surface_height_above_geoid',
     &     'm',REAL_TYPE,precision,fn_kmu2d)
      call base_to_cgs(1,h0(1,1,np0),1)

#ifdef flux_write
      call mnc_write_4d( netcdffile , surf_t_flux(1,1,1) ,
     &     'hfo','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'surface_downward_heat_flux_in_sea_water',
     &     'W/m^2',REAL_TYPE,precision,fn_kmt2d)
      call mnc_write_4d( netcdffile , surf_n_flux(1,1,1) ,
     &     'OasisTFLUX','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'Surface Temp. Flux',
     &     'W/m^2',REAL_TYPE,precision,fn_kmt2d)
      call mnc_write_4d( netcdffile , surf_t_flux(1,1,2) ,
     &     'wfo','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'water_flux_into_ocean',
     &     'm/year',REAL_TYPE,precision,fn_kmt2d)
      call mnc_write_4d( netcdffile , surf_n_flux(1,1,2) ,
     &     'OasisSFLUX','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'Surface Salinity. Flux',
     &     'm/year',REAL_TYPE,precision,fn_kmt2d)
      if(idft_snap.eq.1) then
c     Add scale factor attribute for fluxes if this is the first write.
        call add_scale_off(netcdffile, 'hfo', SCALE_STF1,c0,
     &       REAL_TYPE)
        call add_scale_off(netcdffile, 'OasisTFLUX', SCALE_STF1,c0,
     &       REAL_TYPE)
        call add_scale_off(netcdffile, 'wfo', SCALE_STF2,c0,
     &       REAL_TYPE)
        call add_scale_off(netcdffile, 'OasisSFLUX', SCALE_STF2,c0,
     &       REAL_TYPE)
      endif
#endif
#ifdef conv_write
      call mnc_write_4d( netcdffile , conv(1,1,1) ,
     &     'CONV','DEPTH','LONGITUDE_T','LATITUDE_T','DAY',
     &     'Convective Mixing',
     &     'Number of times',MNC_INT,MNC_INT,fn_kmt3d)
#endif
      call mnc_write_4d( netcdffile , totsec ,
     &     'SECONDS','DAY',' ',' ',' ',
     &     'Elapsed model time',
     &     'seconds',REAL_TYPE,mnc_double,fn_true)

      call mnc_write_4d( netcdffile , years ,
     &     'YEARS','DAY',' ',' ',' ',
     &     'Elapsed model time',
     &     'years',REAL_TYPE,mnc_double,fn_true)

      end

c
c -------------------------------------------------------------------
c
c This function is used to find whether a point is sea (true) or
c land (false) for T-points, with 3 dimensions given
      logical function fn_kmt3d(ip)
      implicit none
c In :
      integer ip(3)
c     Local:
#include "param.h"
#include "levind.h"
      integer k,i,j

      k = ip(1)
      i = ip(2)
      j = ip(3)
      fn_kmt3d = (kmt(i,j).ge.k)

      end !function fn_kmt3d

c This function is used to find whether a point is sea (true) or
c land (false) for U-points, with 3 dimensions given
      logical function fn_kmu3d(ip)
      implicit none
c In :
      integer ip(3)
c     Local:
#include "param.h"
#include "levind.h"
      integer k,i,j

      k = ip(1)
      i = ip(2)
      j = ip(3)
      fn_kmu3d = (kmu(i,j).ge.k)

      end !function fn_kmu3d

c This function is used to find whether a point is sea (true) or
c land (false) for T-points, with 2 dimensions given
      logical function fn_kmt2d(ip)
      implicit none
c In :
      integer ip(2)
c     Local:
#include "param.h"
#include "levind.h"
      integer i,j

      i = ip(1)
      j = ip(2)
      fn_kmt2d = (kmt(i,j).gt.0)
!      fn_kmt2d = (kmt(i,j).eq.0)
!      fn_kmt2d = .true.

      end !function fn_kmt2d

c This function is used to find whether a point is sea (true) or
c land (false) for U-points, with 2 dimensions given
      logical function fn_kmu2d(ip)
      implicit none
c In :
      integer ip(2)
c     Local:
#include "param.h"
#include "levind.h"
      integer i,j

      i = ip(1)
      j = ip(2)
      fn_kmu2d = (kmu(i,j).gt.0)

      end !function fn_kmt2d

c     This function is used to find whether a point is valid (true) or
c     not (false). This version always returns true
      logical function fn_true(ip)
      implicit none
c In :
      integer ip(*)

      fn_true = .true.

      end !function fn_kmt3d

c
c -------------------------------------------------------------------
c
      subroutine mnc_write_4d( filename , field ,VARNAME,
     &  dim1,dim2,dim3,dim4,
     &     longname,units,MODEL_TYPE,FILE_TYPE,fnkmask)
      implicit none
#include "param.h"
#include "netcdf_moma.h"
      character filename*(*)    !netcdf filename to output to
      _MOMA_REAL field(*)       !Data to be written
      character varname*(*)     !variable name
      character dim1*(*),dim2*(*),dim3*(*),dim4*(*)
      character longname*(*)    !long name attribute
      character units*(*)       !units attribute
      integer MODEL_TYPE        !Identifies the type of data in memory
      integer FILE_TYPE         !Identifies the data type to use in file

      logical fnkmask
      external fnkmask

      integer ndims

      character*(MAXNAMELEN) dimnames(4)
c local
      ndims =0
      if(dim1.ne.' ') then
        ndims = ndims+1
        dimnames(ndims) = dim1
      endif
      if(dim2.ne.' ') then
        ndims = ndims+1
        dimnames(ndims) = dim2
      endif
      if(dim3.ne.' ') then
        ndims = ndims+1
        dimnames(ndims) = dim3
      endif
      if(dim4.ne.' ') then
        ndims = ndims+1
        dimnames(ndims) = dim4
      endif

      call mnc_write( filename , field ,VARNAME,
     &     ndims,dimnames,
     &     longname,units,MODEL_TYPE,FILE_TYPE,fnkmask)
      end

c
c -------------------------------------------------------------------
c
c Wrapper routine for more general read subroutine
      subroutine mnc_read_4d( filename , field ,VARNAME,
     &     dim1,dim2,dim3,dim4,
     &     longname,units,MODEL_TYPE,filetype,fnkmask)
      implicit none
#include "param.h"
#include "netcdf_moma.h"
      character filename*(*)    !netcdf filename to output to
      _MOMA_REAL field(*)       !Data to be written
      character varname*(*)     !variable name
      character dim1*(*),dim2*(*),dim3*(*),dim4*(*)
      character longname*(*)    !long name attribute
      integer filetype
      character units*(*)       !units attribute
      integer MODEL_TYPE        !Identifies the type of data in memory
      integer FILE_TYPE         !Identifies the data type used in the file

      logical fnkmask
      external fnkmask

      integer ndims

      character*(MAXNAMELEN) dimnames(4)
c local
      ndims =0
      if(dim1.ne.' ') then
        ndims = ndims+1
        dimnames(ndims) = dim1
      endif
      if(dim2.ne.' ') then
        ndims = ndims+1
        dimnames(ndims) = dim2
      endif
      if(dim3.ne.' ') then
        ndims = ndims+1
        dimnames(ndims) = dim3
      endif
      if(dim4.ne.' ') then
        ndims = ndims+1
        dimnames(ndims) = dim4
      endif

      call mnc_read( filename , field ,VARNAME,
     &     ndims,dimnames,
     &     longname,units,MODEL_TYPE,FILE_TYPE,fnkmask)
      end !subroutine mnc_read_3d

c
c -------------------------------------------------------------------
c
      subroutine mnc_read( filename , field ,VARNAME,
     &  ndims,dimnames,
     &     longname,units,MODEL_TYPE,FILE_TYPE,fnkmask)
#include "param.h"
#include "netcdf_moma.h"
      character filename*(*)    !netcdf filename to output to
      VOIDP field(*)        !Data to be written
      character varname*(*)     !variable name
      integer ndims
      character*(MAXNAMELEN) dimnames(ndims)
      character longname*(*)    !long name attribute
      character units*(*)       !units attribute
      integer MODEL_TYPE        !Identifies the type of data in memory
      integer FILE_TYPE         !Identifies the data type to use in file
      logical fnkmask
      external fnkmask
c local
#include "netcdf.inc"
      integer status,ncid       !status and netcdf file number
      integer varid             !vavriable id number
      integer length(MAXDIMS)   !Length of the axis in memory
      integer ivlen(MAXDIMS)    !Length of data to take
      integer istart(MAXDIMS)   !First value to write from dimension
      integer ipermute(MAXDIMS) !How memory axes map to netcdf axes
      integer opstart(MAXDIMS)  !Where the write to file starts
      VOIDP pfill(8)            !Byte store for fill values in file
      VOIDP pmodelfill(8)       !Byte store for fill values in memory
c     External Functions:
      integer lnblnk            !function for last non-blank character
      external lnblnk

      mnc_lwrite = .false.       !Prevent new dimensions being created

      write(stdout,*)' Reading field "',varname(1:lnblnk(varname)),
     &     '" from file "',filename(1:lnblnk(filename)),'"'

      status = NF_OPEN (filename, NF_WRITE , ncid)
      if(status.ne.NF_NOERR) then
        write(stdout,*)' Error opening the file called "',
     &       filename(1:lnblnk(filename)),'"'
        write(stdout,*)' The file may already exist, or you'
     &       //' may not have permission to write to this location.'
        call moma_nc_err(status,'Opening file')
      endif

      call check_var(ncid,varname,ndims,dimnames,longname,
     &     units,file_type,length,ivlen,istart,opstart,ipermute,varid)

c     Find fill value to use
      call get_fill_val(ncid,varid,MODEL_TYPE,pfill)
      call get_model_fill_val(MODEL_TYPE,pmodelfill)

c     Read file data into buffer.
c
      call mnc_get_data(ncid,ndims,model_type,file_type,
     &     length,ivlen,istart,opstart,ipermute,varid,mnc_bufc1)

c     Copy data from buffer into field, replacing file with model fillvalue
      call buff2model(MODEL_TYPE,mnc_bufc1,field,
     &     ndims,length,pfill,pmodelfill)

c     close file
      status =  nf_close(ncid)
      if(status .ne. nf_NoErr)
     &     call moma_nc_err(status,'Closing file')

      end

c
c -------------------------------------------------------------------
c
      subroutine mnc_write( filename , field ,VARNAME,
     &  ndims,dimnames,
     &     longname,units,MODEL_TYPE,FILE_TYPE,fnkmask)

#include "param.h"
#include "netcdf_moma.h"
      character filename*(*)    !netcdf filename to output to
      VOIDP field(*)        !Data to be written
      character varname*(*)     !variable name
      integer ndims
      character*(MAXNAMELEN) dimnames(ndims)
      character longname*(*)    !long name attribute
      character units*(*)       !units attribute
      integer MODEL_TYPE        !Identifies the type of data in memory
      integer FILE_TYPE         !Identifies the data type to use in file
      logical fnkmask
      external fnkmask
c local
#include "netcdf.inc"
      integer status,ncid       !status and netcdf file number
      integer varid             !vavriable id number
      integer length(MAXDIMS)   !Length of the axis in memory
      integer ivlen(MAXDIMS)    !Length of data to take
      integer istart(MAXDIMS)   !First value to write from dimension
      integer ipermute(MAXDIMS) !How memory axes map to netcdf axes
      integer opstart(MAXDIMS)  !Where the write to file starts
      VOIDP pfill(8)            !Byte store for fill values
c functions:
      integer lnblnk
      external lnblnk

      mnc_lwrite = .true.       !Allow new dimensions to be created

      write(stdout,*)' Writing field "',varname(1:lnblnk(varname)),
     &     '" to file "',filename(1:lnblnk(filename)),'"'

      status = NF_OPEN (filename, NF_WRITE , ncid)
      if(status.ne.NF_NOERR) then
        write(stdout,*)' Error opening the file called "',
     &       filename,'"'
        write(stdout,*)' The file may already exist, or you'
     &       //' may not have permission to write to this location.'
        call moma_nc_err(status,'Opening file')
      endif
c  [Create file if necessary] - Not yet implemented

      call check_var(ncid,varname,ndims,dimnames,longname,
     &     units,file_type,length,ivlen,istart,opstart,ipermute,varid)

c     Find fill value to use
      call get_fill_val(ncid,varid,MODEL_TYPE,pfill)

c     Copy field to buffer area and apply mask.
      call buff_mask(MODEL_TYPE,field,mnc_bufc1,
     &     ndims,length,pfill,fnkmask)
c
c     Write buffered data to file.
c
      call mnc_put_data(ncid,ndims,model_type,file_type,
     &     length,ivlen,istart,opstart,ipermute,varid,mnc_bufc1)

c     close file
      status =  nf_close(ncid)
      if(status .ne. nf_NoErr)
     &     call moma_nc_err(status,'Closing file')

      mnc_lwrite = .false.       !Prevent unwanted file modification
      end


c
c -------------------------------------------------------------------
c
c This routine will check whether dimname is already included in the netcdf file.
c It will then add it if necessary
c The dimension id of the variable is returned
      subroutine check_dim(ncid, dimname , length,istart,ivlen,
     &     opstart,axistype,dimid)
      implicit none
#include "param.h"
#include "netcdf_moma.h"
#include "coord.h"
#include "ctmngr.h"

c     In:
      integer ncid              !netcdf file id
      character dimname*(*)     !dimension name
c     Out:
      integer dimid             !dimension id from file
      integer length            !Length of the dimension for variables in memory
      integer istart            !First value to use for in-memory variables
      integer ivlen             !length of data to take from variables
      integer opstart           !Start point for output in file.
      character axistype        !'i','j','k' or 'l' for x,y,z or t axes
c Local:
      integer DIM_TYPE          !stores the data type id for dimensions
      integer i1,i2,j1,j2,jvlen
     &     ,k1,kvlen            !data lengths
      integer k                 !loop variable
      logical first_time        !True for first call to this routine
      _MOMA_REAL zw_nc(km+1)   !stores data for depth edges
      common / check_dim_com_l / first_time
      common / check_dim_com_r / zw_nc
      data first_time / .true. /

      if(first_time) then
c     Create dimension data that is not already availble in the model
        first_time = .false.
        zw_nc(1) = 0.0d0
        do k=2,km+1
          zw_nc(k) = zw(k-1)
        enddo
      endif
c
c Sort out data sizes:
c
      j1 = jcbeg-1
      j2 = jcend+1
#ifdef cyclic
      i1 = icbeg
      i2 = icend
#else
      i1 = icbeg-1
      i2 = icend+1
#endif
      ivlen = i2 - i1 +1
      jvlen = j2 - j1 +1
      k1 = 1
      kvlen = km
c
c Get data type for dimensions
      DIM_TYPE = REAL_TYPE
c     Try to add dimension
      if(dimname .eq. 'LONGITUDE_T') then
        call mnc_defdim(ncid,'LONGITUDE_T',imt,i1,ivlen, 1,
     &       DIM_TYPE, dimid,'Longitude', 'degrees','i',
     &       ISEVEN,p_up,NOEDGES,xt(i1),REAL_TYPE)
      elseif(dimname .eq. 'LATITUDE_T') then
        call mnc_defdim(ncid,'LATITUDE_T',jmt,j1,jvlen, 1,
     &        DIM_TYPE, dimid,'Latitude', 'degrees','j',
     &       ISEVEN,p_up,NOEDGES,yt(j1),REAL_TYPE)
      elseif(dimname .eq. 'LONGITUDE_U') then
        call mnc_defdim(ncid,'LONGITUDE_U',imt,i1,ivlen, 1,
     &        DIM_TYPE, dimid,'Longitude', 'degrees','i',
     &       ISEVEN,p_up,NOEDGES,xu(i1),REAL_TYPE)
      elseif(dimname .eq. 'LATITUDE_U') then
        call mnc_defdim(ncid,'LATITUDE_U',jmt,j1,jvlen, 1,
     &        DIM_TYPE, dimid,'Latitude', 'degrees','j',
     &       ISEVEN,p_up,NOEDGES,yu(j1),REAL_TYPE)
      elseif(dimname .eq. 'DEPTH') then
c In order to properly use depth we must define the edges too
        call mnc_defdim(ncid,'DEPTH_EDGES',km+1,k1,kvlen+1, 1,
     &        DIM_TYPE,dimid, 'Depth (Edges)', 'cm','k',
     &       NOTEVEN,p_down,NOEDGES,zw_nc(k1),REAL_TYPE)
        call mnc_defdim(ncid,'DEPTH',km,k1,kvlen, 1,
     &        DIM_TYPE, dimid, 'Depth', 'cm','k',
     &       NOTEVEN,p_down,'DEPTH_EDGES',zt(k1),REAL_TYPE)
      elseif(dimname .eq. 'DEPTH_EDGES') then
        call mnc_defdim(ncid,'DEPTH_EDGES',km+1,k1,kvlen+1, 1,
     &        DIM_TYPE,dimid, 'Depth (Edges)', 'cm','k',
     &       NOTEVEN,p_down,NOEDGES,zw_nc(k1),REAL_TYPE)
      elseif(dimname .eq. 'DAY') then
        call mnc_defdim(ncid,'DAY',MNC_UNLIMITED,1,1, 1,
     &        DIM_TYPE,dimid, 'Model Day', 'days', 'l',
     &       NOTEVEN,p_up,NOEDGES,totday,REAL_TYPE)
      else
        write(*,*) 'Could not find how to implement dimension '
     &       //dimname
        STOP
      endif
c     Retreive values from common block
      length = mnc_length
      istart = mnc_ifirst
      ivlen = mnc_ivlen
      opstart = mnc_opstart
      axistype = mnc_axis_type
      end
c
c -------------------------------------------------------------------
c
c
c This routine checks whether a variable is present in a file,
c when writing it is created if necessary
      subroutine check_var(ncid,varname,ndims,dimnames,longname,
     &     units,datatype,length,ivlen,istart,opstart,ipermute,varid)
      implicit none
#include "param.h"
#include "netcdf_moma.h"
#include "netcdf.inc"

c In...
      integer ncid              !netcdf file id
      integer ndims             !number of dimensions this var has
      character*(MAXNAMELEN) dimnames(ndims) !dimension names
      character varname*(*)     !Variable name
      character longname*(*)    !Description
      character units*(*)       !Units
      integer datatype          !data type specification
c     Out:
      integer varid             !netcdf variable id
      integer length(ndims)     !Length of the axis in memory
      integer ivlen(ndims)      !Length of data to take
      integer istart(ndims)     !First value to write from dimension
      integer ipermute(ndims)   !How memory axes map to netcdf axes
      integer opstart(ndims)    !Where the write to file starts
c Local:
      integer status            !netcdf status
      integer idim              !loop variable
      integer dimids(MAXDIMS)   !dimension ids for each dimension
      integer dimid_perm(MAXDIMS) !permuted dimension ids
      character axistype(MAXDIMS) !i,j,k, or l for x,y,z or t axes
      character ctxtstr*100     !context information
      integer nfdatatype        !Netcdf data type
      integer ilnblnk           !last non blank character
c Functions
      integer lnblnk

      ctxtstr = 'Defining variable: '//varname

      do idim=1,ndims
        call check_dim(ncid, dimnames(idim),length(idim),
     &       istart(idim),ivlen(idim),opstart(idim)
     &       ,axistype(idim), dimids(idim))
      enddo


C     Get variable ID number if it's already there!
      status=NF_INQ_VARID(NCID, varname, varid)
      if(status.eq.NF_NOERR) then
c     Check how axes will be permuted when written
        call get_permute(ncid,varid,ndims,dimids,ipermute)
c Return the varid that was found
c We could check at this point whether the variable size in the file
c matches our expectations, but don't at the moment
        return
      endif

      if (.not.mnc_lwrite) then
        write(*,*) ' ERROR - looking to read from variable ',
     &       varname(1:lnblnk(varname)),' but it does not appear ',
     &       'to be in the file'
        STOP
      endif

c Check how axes will be permuted when written
      call mnc_permute(ndims,axistype,ipermute)
c Permute dimension ids
      do idim=1,ndims
        dimid_perm(idim) = dimids(ipermute(idim))
      enddo

      if(datatype.eq.mnc_float) then
        nfdatatype = nf_float
      elseif(datatype.eq.mnc_double) then
        nfdatatype = nf_double
      elseif(datatype.eq.mnc_int) then
        nfdatatype = nf_int
      else
        STOP 'Data type not recognised'
      endif


      STATUS = NF_REDEF(NCID)   ! put in define mode
      if (status .ne. nf_noerr)
     &     call moma_nc_err(status,ctxtstr)

      status = nf_def_var(ncid,varname, nfdatatype,
     &     ndims,dimid_perm, varid)
      if(status .ne. nf_NoErr)
     &     call moma_nc_err(status,ctxtstr)

c DEfine long_name attribute
      ilnblnk=lnblnk(longname)
      if(ilnblnk.gt.0) then
         status = nf_put_att_text(ncid, varid, 'long_name',ilnblnk,
     &        longname)
         if (status .ne. nf_noerr)
     &     call moma_nc_err(status,ctxtstr)
      endif

c DEfine units attribute
      ilnblnk=lnblnk(units)
      if(ilnblnk.gt.0) then
         status = nf_put_att_text(ncid, varid, 'units'
     &        ,ilnblnk, units)
         if (status .ne. nf_noerr)
     &     call moma_nc_err(status,ctxtstr)
      endif

c Define Fill value
      status = NF_PUT_ATT_DOUBLE(ncid, varid, '_FillValue',
     &     nfdatatype,1,FILLVALUER8 )
      if (status .ne. nf_noerr)
     &     call moma_nc_err(status,ctxtstr)

      STATUS = NF_ENDDEF(NCID)   ! End define mode
      if (status .ne. nf_noerr)
     &     call moma_nc_err(status,ctxtstr)
      end

c
c -------------------------------------------------------------------
c
      subroutine mnc_defdim(ncid,varname,length,ifirst,ivlen,
     &     opstart,opdatatype,dimid, longname, unitsstr,
     &     axis_type, EVENFLAG,ldown,edgesstr,pdata,datatype)
      implicit none
#include "param.h"
#include "netcdf_moma.h"
c In...
      integer ncid              !netcdf file id
      integer length            !vector length
      integer ifirst,ivlen      !First index to take, number of points to take
      character *(*) varname,longname,unitsstr !variable name, long descriptive name & units
      character*(*) edgesstr    !string pointing to a variable containing edge data if not ''
      character axis_type       !i,j,k or l - depending in axis type - x,y,z,t
      logical ldown             ! specify positive direction 'down' if true
      logical EvenFlag          !True if data is evenly spaced
      _MOMA_REAL pdata(length)      !points to the data to be used
      integer opdatatype        !data type identifier for dimension in file
      integer opstart           !Index at which to start write for this dimension
      integer datatype          !data type identifier for model data
c Out...
      integer dimid             !dimension id
c Local...
      integer varid             !variable id number
      integer nfdatatype        !netcdf data type identifier for dimension in file
      integer ndims             !number of dimensions ( = 1 )
      integer ilnblnk
      integer status           !Value returned


! functions... & netcdf constants
#include "netcdf.inc"
      integer lnblnk
      character ctxtstr*100

c     Set common block variables
      mnc_length = length
      mnc_ifirst = ifirst
      mnc_ivlen  = ivlen
      mnc_axis_type = axis_type
      mnc_opstart = opstart

      status=NF_INQ_DIMID (NCID,varname, dimid)
      if(.not.mnc_lwrite) then
c     When we are reading get info only - don't tweak the file
        if(length.eq.MNC_UNLIMITED) then
          mnc_opstart = -1
          if(status.eq.NF_NOERR) then
c     Scan this dimension for the value corresponding to the value given.
c     Return -1 if not found
            call find_indx(ncid,dimid,varname,opdatatype,pdata
     &           ,mnc_opstart,.false.)
          endif
          if(mnc_opstart.eq.-1) then
            write(*,*) 'Could not find axis point for this (time?) step'
          endif
          mnc_length = 1
        endif
        return
      endif

      if(status.eq.NF_NOERR) then
c     Return the dimid that was found.
        mnc_dimid  = dimid
        if(length.eq.MNC_UNLIMITED) then
c     Scan this dimension for the value corresponding to the value given.
c     Add the new value if not found
          call find_indx(ncid,dimid,varname,opdatatype,pdata,mnc_opstart
     &         ,.true.)
          mnc_length = 1
        endif
        return
      endif

      ctxtstr = 'Defining dimension : '//varname
      if(opdatatype.eq.mnc_float) then
        nfdatatype = nf_float
      elseif(opdatatype.eq.mnc_double) then
        nfdatatype = nf_double
      elseif(opdatatype.eq.mnc_int)then
        nfdatatype = nf_int
      else
        STOP 'Unknown data type'
      endif

      ndims=1

      STATUS = NF_REDEF(NCID)   ! put in define mode
      if (status .ne. nf_noerr)
     &     call moma_nc_err(status,ctxtstr)

      if(length.eq.MNC_UNLIMITED) then
        status = nf_def_dim(ncid, varname ,nf_unlimited, DimID)
      else
        status = nf_def_dim(ncid, varname ,ivlen, DimID)
      endif
      if (status .ne. nf_noerr)
     &     call moma_nc_err(status,ctxtstr)

      status = nf_def_var(ncid,varname, nfdatatype,
     &     ndims,dimid, varid)
      if(status .ne. nf_NoErr)
     &     call moma_nc_err(status,ctxtstr)

c DEfine long_name attribute
      ilnblnk=lnblnk(longname)
      if(ilnblnk.gt.0) then
         status = nf_put_att_text(ncid, varid, 'long_name',ilnblnk,
     &        longname)
         if (status .ne. nf_noerr)
     &     call moma_nc_err(status,ctxtstr)
      endif

c DEfine units attribute
      ilnblnk=lnblnk(unitsstr)
      if(ilnblnk.gt.0) then
         status = nf_put_att_text(ncid, varid, 'units'
     &        ,ilnblnk, unitsstr)
         if (status .ne. nf_noerr)
     &     call moma_nc_err(status,ctxtstr)
      endif

      if(EvenFlag) then
c     Tell ferret that axis is evenly spaced
         ilnblnk=4
         status = nf_put_att_text(ncid, varid, 'spacing'
     &        ,ilnblnk, 'even')
         if (status .ne. nf_noerr)
     &     call moma_nc_err(status,ctxtstr)
      endif

c DEfine positive direction if necessary
      if(ldown) then
         ilnblnk=4
         status = nf_put_att_text(ncid, varid, 'positive'
     &        ,ilnblnk, 'down')
         if (status .ne. nf_noerr)
     &     call moma_nc_err(status,ctxtstr)
      endif

c Specify edge data
      ilnblnk=lnblnk(edgesstr)
      if(ilnblnk.gt.0) then
         status = nf_put_att_text(ncid, varID,
     &        'edges',ilnblnk, edgesstr)
         if (status .ne. nf_noerr)
     &     call moma_nc_err(status,ctxtstr)
      endif

      STATUS = NF_ENDDEF(NCID)   ! End define mode
      if (status .ne. nf_noerr)
     &     call moma_nc_err(status,ctxtstr)

c     Write data
      if(length.eq.MNC_UNLIMITED) then
c     Add the new value if not found
        call find_indx(ncid,dimid,varname,opdatatype,pdata,mnc_opstart
     &       ,.true.)
        mnc_length = 1
      else

        if(datatype.eq.mnc_float) then
          status = NF_PUT_VAR_REAL(NCID,varID, pdata)
        elseif(datatype.eq.mnc_double) then
          status = NF_PUT_VAR_DOUBLE(NCID,varID, pdata)
        elseif(datatype.eq.mnc_int) then
          status = NF_PUT_VAR_INT (NCID,varID, pdata)
        else
          STOP 'This data type is not implemented for a dimension yet.'
        endif
        if (status .ne. nf_noerr)
     &       call moma_nc_err(status,ctxtstr)
      endif

      mnc_dimid  = dimid

      end !subroutine defdim

c
c -------------------------------------------------------------------
c
c This subroutine creates a new netcdf file with the minimum of information in it.
      subroutine new_netcdf_file(filename)
      implicit none
#include "param.h"
#include "netcdf_moma.h"
#include "netcdf.inc"
      character filename*(*)  !Filename to use when creating

c Local:
      integer ncid              !netcdf file id
      integer status            !status returned from routines

c Create the file, overwriting any old one
c     ordinary netcdf3
c     status = NF_CREATE (filename, NF_CLOBBER,  ncid)
c     ordinary netcdf4
      status = NF_CREATE (filename, OR(NF_CLOBBER, NF_NETCDF4),  ncid)
      if(status.ne.NF_NOERR) then
        write(stdout,*)' Error opening the file called "',
     &       filename,'"'
        write(stdout,*)' The file may already exist, or you'
     &       //' may not have permission to write to this location.'
        STOP 'ERROR'
      endif
c
c Now write the global attributes for the whole file
c
      STATUS = NF_PUT_ATT_INT (NCID, NF_GLOBAL, 'FMODE',
     &      nf_int,1,2)
      if (status .ne. nf_noerr) call moma_nc_err(status,
     &     'Defining global FMODE attribute')
c
      STATUS = NF_PUT_ATT_INT (NCID, NF_GLOBAL, 'FTYPE',
     &     nf_int,1,1)
      if (status .ne. nf_noerr) call moma_nc_err(status,
     &     'Defining global FTYPE attribute')
c
c     set rotation attribute.  1= non-rotated 2=rotated
      STATUS = NF_PUT_ATT_INT (NCID, NF_GLOBAL, 'ROTATION',
     &     nf_int,1,1)
      if (status .ne. nf_noerr) call moma_nc_err(status,
     &     'Defining global ROTATION attribute')

c     set conventions
      STATUS = NF_PUT_ATT_TEXT (NCID, NF_GLOBAL, 'CONVENTIONS',
     &     NCCHAR,'6','CF-1.0',1)
      if (status .ne. nf_noerr) call moma_nc_err(status,
     &     'Defining global CONVENTIONS attribute')

c     End Define mode, close file.
      STATUS =  nf_ENDDEF(ncid)
      if (status .ne. nf_noerr)
     &     call moma_nc_err(status,
     &     'Ending Define Mode in new_netcdf_file')

      STATUS =  nf_close(ncid)
      if (status .ne. nf_noerr)
     &     call moma_nc_err(status,
     &     'Closing netcdf file new_netcdf_file')

      end
c
c -------------------------------------------------------------------
c

c Determine how the axes will be permuted
      subroutine mnc_permute(ndims,axistype,ipermute)
      implicit none
#include "param.h"
#include "netcdf_moma.h"

c In:
      integer ndims !number of dimensions
      character axistype(ndims) !i,j,k or l
c Out:
      integer ipermute(ndims)
c Local:
      integer i,itemp
      integer iaxistype(MAXDIMS)
      integer nchanges
c Convert axis types to values and store no-permutation
      do i=1,ndims
        iaxistype(i) = ichar(axistype(i)) - ichar('i') +1
        ipermute(i) = i
      enddo
c Just do a sort on the axis type, tracking changes in ipermute
      nchanges =1
      do while (nchanges.gt.0)
        nchanges = 0
        do i=1,ndims-1
          if(iaxistype(i).gt.iaxistype(i+1)) then
c     Swap out of order axes...
            itemp = iaxistype(i)
            iaxistype(i) = iaxistype(i+1)
            iaxistype(i+1) = itemp
            itemp = ipermute(i)
            ipermute(i) = ipermute(i+1)
            ipermute(i+1) = itemp
            nchanges = nchanges+1
          endif
        enddo                   !do i=1,ndims-1
      enddo                     !do while ...

      end                       ! subroutine mnc_permute

c
c -------------------------------------------------------------------
c
c Copy field to array and apply a mask.
      subroutine buff_mask(MODEL_TYPE,infield,outfield,ndims,
     &     length,pmaskval, fnkmask)
      implicit none
#include "param.h"
#include "netcdf_moma.h"
      integer MODEL_TYPE !model data type
      VOIDP infield(*) ,outfield(*) !
      integer ndims
      integer length(ndims)
      VOIDP pmaskval(*)
      logical fnkmask
      external fnkmask
c Local:
      integer cplen             !length of one element in array
      integer ip(MAXDIMS)       !position (i,j,k,l)
      integer ivp               !output pointer
      integer totlen            !total output length
      integer inext             !Used to increment pointer

c     Get length of a single data element
      call datatype_len(MODEL_TYPE,cplen)

      totlen = cplen
      ip(1) = 1                 !For when ndims=0
      do ivp = 1,ndims
        totlen = totlen*length(ivp)
        ip(ivp) = 1
      enddo

      do ivp = 1,totlen,cplen
        if(fnkmask(ip)) then
          call copy_bytes(outfield(ivp),infield(ivp),cplen)
        else
          call copy_bytes(outfield(ivp),pmaskval,cplen)
        endif

        inext=1
        do while(inext.gt.0.and.(inext.le.ndims))
          ip(inext) = ip(inext)+1
          if(ip(inext).gt.length(inext)) then
c     Carry over to next dimension
            ip(inext) = 1
            inext = inext +1
          else
            inext = 0 !no carry over - done
          endif
        enddo
      enddo

      end ! subroutine buff_mask

c
c -------------------------------------------------------------------
c
c Copy field to array and apply a mask.
      subroutine buff2model(MODEL_TYPE,infield,outfield,ndims,
     &     length,pmaskval, pnewmask)
      implicit none
#include "param.h"
#include "netcdf_moma.h"
      integer MODEL_TYPE !model data type
      VOIDP infield(*) ,outfield(*) !
      integer ndims
      integer length(ndims)
      VOIDP pmaskval(*), pnewmask(*)
c Local:
      integer cplen             !length of one element in array
      integer ip(MAXDIMS)       !position (i,j,k,l)
      integer ivp               !output pointer
      integer totlen            !total output length
      logical ismatch

c     Get length of a single data element
      call datatype_len(MODEL_TYPE,cplen)

      totlen = cplen
      ip(1) = 1                 !For when ndims=0
      do ivp = 1,ndims
        totlen = totlen*length(ivp)
        ip(ivp) = 1
      enddo

      do ivp = 1,totlen,cplen
        call isbytematch(cplen,infield(ivp),pmaskval,ismatch)
        if(ismatch) then
          call copy_bytes(outfield(ivp),pnewmask,cplen)
        else
          call copy_bytes(outfield(ivp),infield(ivp),cplen)
        endif
      enddo

      end ! subroutine buff_mask

      subroutine copy_bytes(dest,src,ilen)
      implicit none
c     In
      VOIDP src(*)
      integer ilen
c     Out
      VOIDP dest(*)
c     local
      integer i
      do i=1,ilen
        dest(i) = src(i)
      enddo
      end

c
c -------------------------------------------------------------------
c
c This routine transfers data from the given field to the netcdf file
      subroutine mnc_put_data(ncid,ndims,model_type,datatype,
     &     length,ivlen,istart,opstart,ipermute,varid,field)
      implicit none
#include "param.h"
#include "netcdf_moma.h"
#include "netcdf.inc"
c In...
      integer ncid              !netcdf file id
      integer ndims             !number of dimensions this var has
      integer model_type        !Data type in memory
      integer datatype          !data type specification in file
      integer varid             !netcdf variable id
      integer length(ndims)     !Length of the axis in memory
      integer ivlen(ndims)      !Length of data to take
      integer istart(ndims)     !First value to write from dimension
      integer ipermute(ndims)   !How memory axes map to netcdf axes
      integer opstart(ndims)    !Where the write starts from.
      VOIDP field(*)        !Start of data...
c Local
      integer status            !netcdf status
      integer ipfirst           !First valid data
      integer start(MAXDIMS),   !netcdf write start point
     &     count(MAXDIMS),      !number of netcdf writes
     &     stride(MAXDIMS),     !step between netcdf writes
     &     imap(MAXDIMS)        !Maps in memory to in-file data
      integer i                 !loop counter
      integer icount(MAXDIMS)   !Memory stride for each dimension
      integer cplen             !Length of one data element in bytes

      do i=1,ndims
        start(i) = opstart(ipermute(i))
        stride(i) = 1
        count(i) = ivlen(ipermute(i))
      enddo

      icount(1) = 1
      do i=2,ndims
        icount(i) = icount(i-1)*length(i-1)
      enddo

c     create imap array
      do i=1,ndims
        imap(i) = icount(ipermute(i))
      enddo

c     Locate first data point
c     Get length of a single data element
      call datatype_len(MODEL_TYPE,cplen)

      ipfirst = 0
      do i=1,ndims
        ipfirst = ipfirst + (istart(i)-1)*icount(i)
      enddo
c Convert to byte ofset...
      ipfirst = ipfirst*cplen +1

      if(MODEL_TYPE.eq.mnc_float) then
        STATUS = NF_PUT_VARM_REAL(ncid,varid, start, count, stride,
     &       imap, field(ipfirst))
      elseif(MODEL_TYPE.eq.mnc_int) then
        STATUS = NF_PUT_VARM_INT(ncid,varid, start, count, stride,
     &       imap, field(ipfirst))
      elseif(MODEL_TYPE.eq.mnc_double) then
        STATUS = NF_PUT_VARM_DOUBLE(ncid,varid, start, count, stride,
     &       imap, field(ipfirst))
      else
        STOP 'Invalid data type ...'
      endif
      if (status .ne. nf_noerr) call moma_nc_err(status,
     &     'Writing mapped variable')

      end !subroutine mnc_put_data
c
c -------------------------------------------------------------------
c
      subroutine mnc_get_data(ncid,ndims,model_type,datatype,
     &     length,ivlen,istart,opstart,ipermute,varid,field)
      implicit none
#include "param.h"
#include "netcdf_moma.h"
#include "netcdf.inc"
c In...
      integer ncid              !netcdf file id
      integer ndims             !number of dimensions this var has
      integer model_type        !Data type in memory
      integer datatype          !data type specification in file
      integer varid             !netcdf variable id
      integer length(ndims)     !Length of the axis in memory
      integer ivlen(ndims)      !Length of data to take
      integer istart(ndims)     !First value to write from dimension
      integer ipermute(ndims)   !How memory axes map to netcdf axes
      integer opstart(ndims)    !Where the write starts from.
      VOIDP field(*)        !Start of data...
c Local
      integer status            !netcdf status
      integer ipfirst           !First valid data
      integer start(MAXDIMS),   !netcdf write start point
     &     count(MAXDIMS),      !number of netcdf writes
     &     stride(MAXDIMS),     !step between netcdf writes
     &     imap(MAXDIMS)        !Maps in memory to in-file data
      integer i                 !loop counter
      integer icount(MAXDIMS)   !Memory stride for each dimension
      integer cplen             !Length of one data element in bytes

      do i=1,ndims
        start(i) = opstart(ipermute(i))
        stride(i) = 1
        count(i) = ivlen(ipermute(i))
      enddo

      icount(1) = 1
      do i=2,ndims
        icount(i) = icount(i-1)*length(i-1)
      enddo

c     create imap array
      do i=1,ndims
        imap(i) = icount(ipermute(i))
      enddo

c     Locate first data point
c     Get length of a single data element
      call datatype_len(MODEL_TYPE,cplen)

      ipfirst = 0
      do i=1,ndims
        ipfirst = ipfirst + (istart(i)-1)*icount(i)
      enddo
c Convert to byte ofset...
      ipfirst = ipfirst*cplen +1

      if(MODEL_TYPE.eq.mnc_float) then
        STATUS = NF_GET_VARM_REAL(ncid,varid, start, count, stride,
     &       imap, field(ipfirst))
      elseif(MODEL_TYPE.eq.mnc_int) then
        STATUS = NF_GET_VARM_INT(ncid,varid, start, count, stride,
     &       imap, field(ipfirst))
      elseif(MODEL_TYPE.eq.mnc_double) then
        STATUS = NF_GET_VARM_DOUBLE(ncid,varid, start, count, stride,
     &       imap, field(ipfirst))
      else
        STOP 'Invalid data type ...'
      endif
      if (status .ne. nf_noerr) call moma_nc_err(status,
     &     'Writing mapped variable')

      end !subroutine mnc_get_data
c
c -------------------------------------------------------------------
c

c
c     Find which index in the dimension  matches the value given
      subroutine find_indx(ncid,dimid,varname,datatype,pval,indx,ladd)
      implicit none
#include "param.h"
#include "netcdf_moma.h"
#include "netcdf.inc"
c     In:
      integer ncid              !netcdf file id
      integer dimid             !dimension id to scan
      integer datatype
      character varname*(*)
      VOIDP pval(*)         !Pointer to value
      logical ladd              !Another index is added if this is true
c     Out:
      integer indx              !output point (-1 if not found)
c     Local:
      integer len               !dimension length
      integer status
      integer i,varid           !loop counter , variable id
      VOIDP pbuf(8)         !local buffer for getting value to
      integer cplen             !length of a single data item
      logical ismatch           !True when a matching point is found

      status = NF_INQ_DIMLEN(ncid,dimid,len)
      if (status .ne. nf_noerr) call moma_nc_err(status,
     &     'Getting dimension info')

      status = NF_INQ_VARID(ncid,varname,varid)
      if (status .ne. nf_noerr) call moma_nc_err(status,
     &     'Getting variable from dimension name')

c Get length of a single data element
      call datatype_len(datatype,cplen)

      do i=1,len
        if(datatype.eq.mnc_float) then
          status = nf_get_var1_real(ncid,varid,i,pbuf)
        elseif(datatype.eq.mnc_double) then
          status = nf_get_var1_double(ncid,varid,i,pbuf)
        elseif(datatype.eq.mnc_int) then
          status = nf_get_var1_int(ncid,varid,i,pbuf)
        endif
        if (status .ne. nf_noerr) call moma_nc_err(status,
     &       'Getting variable value in dimension')
        call isbytematch(cplen,pbuf,pval,ismatch)
        if(ismatch) then
          indx = i
          return
        endif

      enddo

c     Not found if we get here...
      if(ladd) then
c     Create a new point on the dimension
        indx = len + 1
        i = indx
        if(datatype.eq.mnc_float) then
          status = nf_put_var1_real(ncid,varid,i,pval)
        elseif(datatype.eq.mnc_double) then
          status = nf_put_var1_double(ncid,varid,i,pval)
        elseif(datatype.eq.mnc_int) then
          status = nf_put_var1_int(ncid,varid,i,pval)
        endif
        if (status .ne. nf_noerr) call moma_nc_err(status,
     &       'Putting value in unlimited dimension')
      else
c     Return -1
        indx = -1
      endif

      end !subroutine find_indx
c
c -------------------------------------------------------------------
c


      subroutine datatype_len(datatype,cplen)
      implicit none
      integer datatype          !a valid data type
      integer cplen               !the length of a single element in bytes
#include "param.h"
#include "netcdf_moma.h"

      if(datatype.eq.mnc_float) then
        cplen = SIZEOF_MNC_FLOAT
      elseif(datatype.eq.mnc_double) then
        cplen = SIZEOF_MNC_DOUBLE
      elseif(datatype.eq.mnc_int) then
        cplen = SIZEOF_MNC_INT
      else
        STOP 'Type invalid'
      endif
      end !subroutine datatype_len
c
c -------------------------------------------------------------------
c

c     Check for a byte for byte match between two values
      subroutine isbytematch(len,pval1,pval2,ismatch)
      implicit none
c In
      integer len               !length of match to test
      VOIDP pval1(*),pval2(*)   !byte streams to test
c Out
      logical ismatch           !True if they match, false otherwise
c Local
      integer i

      ismatch = .false.
      do i=1,len
        if(pval1(i).ne.pval2(i)) then
          return
        endif
      enddo
      ismatch = .true.

      end !subroutine isbytematch
c
c -------------------------------------------------------------------
c

      SUBROUTINE MOMA_NC_ERR(STATUS,context)
      IMPLICIT none
      INTEGER STATUS
      character*(*)context
#include "netcdf.inc"
#include "param.h"

      IF (STATUS .NE. NF_NOERR) THEN
       WRITE(stdout,*) 'Netcdf Error:'
       WRITE(stdout,*) NF_STRERROR(STATUS)
       WRite(stdout,*) context
       STOP 'Stopped'
      ENDIF
      END !SUBROUTINE MOMA_NC_ERR
c
c -------------------------------------------------------------------
c

      subroutine add_scale_off(netcdffile, varname, pscale,poff,
     &     MODEL_TYPE)
      implicit none
      character*(*) netcdffile  !file name to apply to
      character*(*) varname     !variable name in file
      VOIDP pscale,poff         !Scale and offset values
      integer MODEL_TYPE        !Data type of pscale and poff
#include "param.h"
#include "netcdf_moma.h"
#include "netcdf.inc"
c     local
      integer vartype           !data type of variable
      integer status            !status from netcdf routines
      integer ncid              !file id
      integer varid             !variable id
      REAL*8  doff,dscale,newfillvalue

      status = NF_OPEN (netcdffile, NF_WRITE , ncid)
      if(status.ne.NF_NOERR) then
        write(stdout,*)' Error opening the file called "',
     &       netcdffile,'"'
        write(stdout,*)' The file may already exist, or you'
     &       //' may not have permission to write to this location.'
        call moma_nc_err(status,'Opening file for scale & offset')
      endif

      status = NF_INQ_VARID(ncid,varname,varid)
      if (status .ne. nf_noerr) call moma_nc_err(status,
     &     'Getting variable name for scale & offset')

      status = NF_INQ_VARTYPE(ncid,varid,vartype)
      if (status .ne. nf_noerr) call moma_nc_err(status,
     &     'Getting variable type for scale & offset')

      status = NF_REDEF(ncid)
      if (status .ne. nf_noerr) call moma_nc_err(status,
     &     'Entering define mode for scale & offset')

      if(MODEL_TYPE.eq.mnc_int) then
        status = nf_put_att_int(ncid, Varid, 'scale_factor'
     &       ,vartype,1,pscale)
      elseif(MODEL_TYPE.eq.mnc_float) then
        status = nf_put_att_real(ncid, Varid, 'scale_factor'
     &       ,vartype,1,pscale)
      elseif(MODEL_TYPE.eq.mnc_double) then
        status = nf_put_att_double(ncid, Varid, 'scale_factor'
     &       ,vartype,1,pscale)
      endif
      if (status .ne. nf_noerr)
     &     call moma_nc_err(status,'Putting scale factor')

      if(MODEL_TYPE.eq.mnc_int) then
        status = nf_put_att_int(ncid, Varid, 'add_offset'
     &       ,vartype,1,poff)
      elseif(MODEL_TYPE.eq.mnc_float) then
        status = nf_put_att_real(ncid, Varid, 'add_offset'
     &       ,vartype,1,poff)
      elseif(MODEL_TYPE.eq.mnc_double) then
        status = nf_put_att_double(ncid, Varid, 'add_offset'
     &       ,vartype,1,poff)
      endif
      if (status .ne. nf_noerr)
     &     call moma_nc_err(status,'Putting offset')

c *** Commented: amending/writing _FillValue after data are written to a
c                variable is not permitted/returns an error with netcdf4
c     status = nf_get_att_double(ncid, Varid, 'scale_factor',dscale)
c     if (status .ne. nf_noerr)
c    &     call moma_nc_err(status,'Getting scale back')
c     status = nf_get_att_double(ncid, Varid, 'add_offset',doff)
c     if (status .ne. nf_noerr)
c    &     call moma_nc_err(status,'Getting offset back')
c     status = nf_get_att_double(ncid, Varid, '_FillValue',newfillvalue)
c     if (status .eq. nf_noerr)  then
c       newfillvalue = newfillvalue*dscale + doff
c       status = nf_put_att_double(ncid, Varid, '_FillValue',
c    &       vartype,1,newfillvalue)
c       if (status .ne. nf_noerr)
c    &       call moma_nc_err(status,'Amending fill value with scale')
c     endif

      STATUS = NF_ENDDEF(NCID)   ! End define mode
      if (status .ne. nf_noerr)
     &     call moma_nc_err(status,
     &     'Ending define mode after adding scale and offset')


      STATUS = NF_CLOSE(NCID)   ! Close file
      if (status .ne. nf_noerr)
     &     call moma_nc_err(status,
     &     'Closing file after adding scale and offset')

      end ! subroutine add_scale_off
c
c -------------------------------------------------------------------
c

      subroutine get_fill_val(ncid,varid,MODEL_TYPE,pfill)
      implicit none
      integer varid,ncid
      integer MODEL_TYPE
      VOIDP pfill(*)
#include "param.h"
#include "netcdf_moma.h"
#include "netcdf.inc"
c Local
      VOIDP pbuff(8)
      integer ival
      REAL*8 r8val
      real*4 r4val
      equivalence(pbuff,ival,r8val,r4val)
      integer status
      real*8 dscale,doff

      status = nf_get_att_double(ncid, Varid, 'scale_factor',dscale)
      if (status .ne. nf_noerr)
     &     dscale = 1.0
      status = nf_get_att_double(ncid, Varid, 'add_offset',doff)
      if (status .ne. nf_noerr)
     &     doff = 0.0

      if(MODEL_TYPE.eq.mnc_float) then
        status=nf_get_att_real(ncid,varid,'_FillValue',pbuff)
        r4val = (r4val - doff)/dscale
        call copy_bytes(pfill,pbuff,SIZEOF_MNC_FLOAT)
      elseif(MODEL_TYPE.eq.mnc_int) then
        status=nf_get_att_int(ncid,varid,'_FillValue',pbuff)
        ival = (dble(ival) - doff)/dscale
        call copy_bytes(pfill,pbuff,SIZEOF_MNC_INT)
      elseif(MODEL_TYPE.eq.mnc_double) then
        status=nf_get_att_double(ncid,varid,'_FillValue',pbuff)
        r8val = (dble(r8val) - doff)/dscale
        call copy_bytes(pfill,pbuff,SIZEOF_MNC_DOUBLE)
      else
        STOP 'Invalid data type getting fill value...'
      endif

      if(status.eq.nf_noerr) return !Got value so return
c     try missing_value
      if(MODEL_TYPE.eq.mnc_float) then
        status=nf_get_att_real(ncid,varid,'missing_value',pbuff)
        r4val = (r4val - doff)/dscale
        call copy_bytes(pfill,pbuff,SIZEOF_MNC_FLOAT)
      elseif(MODEL_TYPE.eq.mnc_int) then
        status=nf_get_att_int(ncid,varid,'missing_value',pbuff)
        ival = (dble(ival) - doff)/dscale
        call copy_bytes(pfill,pbuff,SIZEOF_MNC_INT)
      elseif(MODEL_TYPE.eq.mnc_double) then
        status=nf_get_att_double(ncid,varid,'missing_value',pbuff)
        r8val = (dble(r8val) - doff)/dscale
        call copy_bytes(pfill,pbuff,SIZEOF_MNC_DOUBLE)
      else
        STOP 'Invalid data type getting fill value...'
      endif

      if(status.eq.nf_noerr) return !Got value so return

c Default values:
      if(MODEL_TYPE.eq.mnc_float) then
        call copy_bytes(pbuff,FILLVALUER4,SIZEOF_MNC_FLOAT)
        r4val = (r4val - doff)/dscale
        call copy_bytes(pfill,pbuff,SIZEOF_MNC_FLOAT)
      elseif(MODEL_TYPE.eq.mnc_int) then
        call copy_bytes(pbuff,FILLVALUEI4,SIZEOF_MNC_INT)
        ival = (dble(ival) - doff)/dscale
        call copy_bytes(pfill,pbuff,SIZEOF_MNC_INT)
      elseif(MODEL_TYPE.eq.mnc_double) then
        call copy_bytes(pbuff,FILLVALUER8,SIZEOF_MNC_DOUBLE)
        r8val = (dble(r8val) - doff)/dscale
        call copy_bytes(pfill,pbuff,SIZEOF_MNC_DOUBLE)
      else
        STOP 'Invalid data type getting fill value...'
      endif

      end !get_fill_val
c
c -------------------------------------------------------------------
c

c Subroutine to return the fill value used in the model fields
      subroutine get_model_fill_val(MODEL_TYPE,pfill)
      implicit none
      integer MODEL_TYPE
      VOIDP pfill(8)
#include "param.h"
#include "netcdf_moma.h"
c Local

c Default values:
      if(MODEL_TYPE.eq.mnc_float) then
        call copy_bytes(pfill,R4ZERO,SIZEOF_MNC_FLOAT)
      elseif(MODEL_TYPE.eq.mnc_int) then
        call copy_bytes(pfill,I4ZERO,SIZEOF_MNC_INT)
      elseif(MODEL_TYPE.eq.mnc_double) then
        call copy_bytes(pfill,R8ZERO,SIZEOF_MNC_DOUBLE)
      else
        STOP 'Invalid data type getting fill value...'
      endif

      end !get_model_fill_val
c
c -------------------------------------------------------------------
c

c Subroutine to check the permutation of axes applied within a netcdf
c file
      subroutine get_permute(ncid,varid,ndims,dimids,ipermute)
      implicit none
      integer ncid              !netcdf file id
      integer varid             !netcdf  variable
      integer ndims             !number of dimensions this variable has
      integer dimids(ndims)     !netcdf dimension ids in memory order
      integer ipermute(ndims)  !Axis permutation
#include "param.h"
#include "netcdf_moma.h"
#include "netcdf.inc"
c Local
      integer filedimid(MAXDIMS) !netcdf dimension ids in file order
      integer filendims         !number of dimensions for variable in file
      integer status
      integer idim,jdim

      status = nf_inq_varndims(ncid,varid,filendims)
      if (status .ne. nf_noerr)  call moma_nc_err(status,
     &     'Getting number of dimensions for var')
      if(ndims.ne.filendims) then
        write(*,*)' ERROR - number of dimensions does not match : '
     &  ,'File : ',filendims,' Expected : ',ndims
        STOP
      endif

      status = nf_inq_vardimid(ncid,varid,filedimid)
      if (status .ne. nf_noerr)  call moma_nc_err(status,
     &     'Getting dimension ids for var.')

      do jdim=1,ndims
        do idim=1,ndims
          if(dimids(idim).eq.filedimid(jdim)) ipermute(jdim)=idim
        enddo
      enddo
      end
c
c -------------------------------------------------------------------
c

c Subroutine to fill halo points with 0 and copy cyclic bcs
      subroutine boundaries(kdim,field)
      implicit none
      integer kdim
#include "param.h"
      _MOMA_REAL field(kdim,imt,jmt)
c local:
      integer i,j,k
     &     ,jn,js
     &     ,jhn,jhs
     &     ,ie,iw
     &     ,ihe,ihw
c
c     South and North Halo
      do j=1,nhalo-1
        js = j
        jhs = nhalo
        jn = jmt - j +1
        jhn = jmt -nhalo + 1
        do i=1,imt
          do k=1,kdim
c     South Halo
            field(k,i,js) = field(k,i,jhs)
c     North Halo
            field(k,i,jn) = field(k,i,jhn)
          enddo
        enddo
      enddo

#ifdef cyclic
c
c     East and West cyclic
      do j=1,jmt
        do i=1,nhalo
          iw = i
          ihw = imt - 2*nhalo + i
          ie = imt - nhalo + i
          ihe = nhalo + i
          do k=1,kdim
c     West Halo
            field(k,iw,j) = field(k,ihw,j)
c     East Halo
            field(k,ie,j) = field(k,ihe,j)
          enddo
        enddo
      enddo
#else
c
c     East and West Solid Halo
      do j=1,jmt
        do i=1,nhalo-1
          iw = i
          ihw = nhalo
          ie = imt - i + 1
          ihe = imt - nhalo + 1
          do k=1,kdim
c     West Halo
            field(k,iw,j) = field(k,ihw,j)
c     East Halo
            field(k,ie,j) = field(k,ihe,j)
          enddo
        enddo
      enddo

#endif
      end
c
c -------------------------------------------------------------------
c

c Subroutine to fill halo points with 0 and copy cylic bcs
      subroutine boundarieskm(field)
      implicit none
#include "param.h"
      integer field(imt,jmt)
c local:
      integer i,j
     &     ,jn,js
     &     ,jhn,jhs
     &     ,ie,iw
     &     ,ihe,ihw
c
c     South and North Halo
      do j=1,nhalo-1
        js = j
        jhs = nhalo
        jn = jmt - j +1
        jhn = jmt -nhalo + 1
        do i=1,imt
c     South Halo
          field(i,js) = field(i,jhs)
c     North Halo
          field(i,jn) = field(i,jhn)
        enddo
      enddo

#ifdef cyclic
c
c     East and West cyclic
      do j=1,jmt
        do i=1,nhalo
          iw = i
          ihw = imt - 2*nhalo + i
          ie = imt - nhalo + i
          ihe = nhalo + i
c     West Halo
            field(iw,j) = field(ihw,j)
c     East Halo
            field(ie,j) = field(ihe,j)
        enddo
      enddo
#else
c
c     East and West Solid Halo
      do j=1,jmt
        do i=1,nhalo-1
          iw = i
          ihw = nhalo
          ie = imt - i + 1
          ihe = imt - nhalo + 1
c     West Halo
            field(iw,j) = field(ihw,j)
c     East Halo
            field(ie,j) = field(ihe,j)
        enddo
      enddo

#endif
      end
c
c -------------------------------------------------------------------
c
c     Check for non-zero values at land (if lchkland) and /or
c     check for zero values on sea points (if lchksea)
      subroutine check_field(kdim,field,kmask,lchkland,lchksea,lerr)
      implicit none
#include "param.h"
      integer kdim !size of first dimension
      _MOMA_REAL field(kdim,imt,jmt) !The field to check
      integer kmask(imt,jmt) !the mask (kmt or imt)
      logical lchkland, lchksea
c Out:
      logical lerr              !goes true if a mismatch is found
c Local
      integer i,j,k
      if(lchkland) then
        do j=1,jmt
          do i=1,imt
            do k=kmask(i,j)+1,kdim
              if(field(k,i,j).ne.c0) then
                write(*,*)'   WARNING field is non-zero on a land ',
     &               'point at i=',i,' j=',j,' k=',k
                field(k,i,j)=c0
                lerr = .true.
              endif
            enddo
          enddo
        enddo
      endif

      if(lchksea) then
        do j=1,jmt
          do i=1,imt
            do k=1,min(kdim,kmask(i,j))
              if(field(k,i,j).eq.c0) then
                write(*,*)'   WARNING : field is exactly 0.0 on a sea ',
     &               'point at i=',i,' j=',j,' k=',k
                lerr = .true.
              endif
            enddo
          enddo
        enddo
      endif
      end

#else
c Some compilers do not like empty files
c So a dummy subroutine is included here
      subroutine undefed_netcdf
      end
#endif /*netcdf*/

c
c -------------------------------------------------------------------
c

c Subroutine to change variables to base units (from cgs)
      subroutine cgs_to_base(kdim,field,form)
      implicit none
      integer kdim
#include "param.h"
      _MOMA_REAL field(kdim,imt,jmt)
c local:
      integer i,j,k,form

      if(form.eq.0) then
      do j=1,jmt
        do i=1,imt
          do k=1,kdim
           if(field(k,i,j).ne.0) then
             field(k,i,j)=field(k,i,j)+273.16
           endif
          enddo
        enddo
      enddo
      endif

      if(form.eq.1) then
      do j=1,jmt
        do i=1,imt
          do k=1,kdim
           if(field(k,i,j).ne.0) then
             field(k,i,j)=field(k,i,j)*0.01
           endif
          enddo
        enddo
      enddo
      endif

      if(form.eq.2) then
      do j=1,jmt
        do i=1,imt
          do k=1,kdim
           if(field(k,i,j).ne.0) then
             field(k,i,j)=(field(k,i,j)*1000)+35
           endif
          enddo
        enddo
      enddo
      endif

c----For salt tendency terms----
      if(form.eq.3) then
      do j=1,jmt
        do i=1,imt
          do k=1,kdim
           if(field(k,i,j).ne.0) then
             field(k,i,j)=(field(k,i,j)*1000)
           endif
          enddo
        enddo
      enddo
      endif

      if(form.eq.4) then
      do j=1,jmt
        do i=1,imt
          do k=1,kdim
           if(field(k,i,j).ne.0) then
             field(k,i,j)=field(k,i,j)*10.
           endif
          enddo
        enddo
      enddo
      endif

      end
c
c -------------------------------------------------------------------
c

c Subroutine to change variables back to cgs units (from cgs)
      subroutine base_to_cgs(kdim,field,form)
      implicit none
      integer kdim
#include "param.h"
      _MOMA_REAL field(kdim,imt,jmt)
c local:
      integer i,j,k,form

      if(form.eq.0) then
      do j=1,jmt
        do i=1,imt
          do k=1,kdim
           if(field(k,i,j).ne.0) then
             field(k,i,j)=field(k,i,j)-273.16
           endif
          enddo
        enddo
      enddo
      endif

      if(form.eq.1) then
      do j=1,jmt
        do i=1,imt
          do k=1,kdim
           if(field(k,i,j).ne.0) then
             field(k,i,j)=field(k,i,j)*100
           endif
          enddo
        enddo
      enddo
      endif

      if(form.eq.2) then
      do j=1,jmt
        do i=1,imt
          do k=1,kdim
           if(field(k,i,j).ne.0) then
             field(k,i,j)=(field(k,i,j)-35)*0.001
            endif
         enddo
        enddo
      enddo
      endif

c----For salt tendency terms----
      if(form.eq.3) then
      do j=1,jmt
        do i=1,imt
          do k=1,kdim
           if(field(k,i,j).ne.0) then
             field(k,i,j)=(field(k,i,j)*0.001)
           endif
          enddo
        enddo
      enddo
      endif

      if(form.eq.4) then
      do j=1,jmt
        do i=1,imt
          do k=1,kdim
           if(field(k,i,j).ne.0) then
             field(k,i,j)=field(k,i,j)*0.1
           endif
          enddo
        enddo
      enddo
      endif

      end
