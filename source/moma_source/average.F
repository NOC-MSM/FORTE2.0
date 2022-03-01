#include "cpp_options.h"
#ifdef average
c
c This file contains routines to perform time averaging on the model fields
c
c
c     this routine sets all the time averaging fields to zero
      subroutine av_zero
      implicit none
#include "param.h"
#include "average.h"

c local:
      integer i,j,k,it

C$OMP PARALLEL DO PRIVATE( i , j ) , SCHEDULE(STATIC)
      do j=1,jmt
        do i=1,imt
          do it=1,nt
            do k=1,km
              avt(k,i,j,it) = c0
#ifdef tendency_write
              tend_adv_av(k,i,j,it) = c0
              tend_hdiff_av(k,i,j,it) = c0
              tend_vdiff_av(k,i,j,it) = c0
#endif /*tendency_write*/
#ifdef nflux_write
              avnflux(k,i,j,it) = c0
#endif /*nflux_write*/
            enddo
#ifdef flux_write
            avstf(i,j,it) = c0
            avsmf(i,j,it) = c0
            avsnf(i,j,it) = c0
#endif /*flux_write*/
          enddo

          do k=1,km
            avu(k,i,j) = c0
            avv(k,i,j) = c0
#ifdef rho_is_write
            rho_av(k,i,j) = c0
#endif /*rho_is_write*/
#ifdef ekin_write
            ekin(k,i,j) = c0
#endif /*ekin_write*/
#ifdef conv_write
            avconv(k,i,j) = c0
#endif /*conv_write*/
#ifdef c_write
            avcs(k,i,j) = c0
#endif /*c_write*/
          enddo
          avu0(i,j) = c0
          avv0(i,j) = c0
          avh0(i,j) = c0
#ifdef ice_write
          avice(i,j) = c0
#endif /*ice_write*/
#ifdef w_write
          do k=0,km
            avw(k,i,j) = c0
          enddo
#endif /*w_write*/
        enddo
      enddo
C$OMP END PARALLEL DO

      avtime = c0
      end

c     This routine cumulates the time averaged fields...
      subroutine av_cumulate( timeinc )
      implicit none
#include "param.h"
#include "average.h"
#include "timelv.h"
#include "slabs.h"
#include "frees.h"
#include "cvbc.h"
#include "ctmngr.h"
#include "grdvar.h"

c In:
      _MOMA_REAL timeinc !Time increment (Weighting)
c The units of timeinc are not important, as long as you are consistent
c when calling this routine

c local:
      integer i,j,k,it

C$OMP PARALLEL DO PRIVATE( i , j ) , SCHEDULE(STATIC)
      do j=1,jmt
        do i=1,imt

          do k=1,km

            do it=1,nt
              avt(k,i,j,it) = avt(k,i,j,it) + t(k,i,j,it,np)*timeinc
#ifdef tendency_write
              tend_adv_av(k,i,j,it) = tend_adv_av(k,i,j,it)
     &         + tend_adv(k,i,j,it)*timeinc
              tend_hdiff_av(k,i,j,it) = tend_hdiff_av(k,i,j,it)
     &         + tend_hdiff(k,i,j,it)*timeinc
              tend_vdiff_av(k,i,j,it) = tend_vdiff_av(k,i,j,it)
     &         + tend_vdiff(k,i,j,it)*timeinc
#endif /*tendency_write*/
            enddo

            avu(k,i,j) = avu(k,i,j) + u(k,i,j,np)*timeinc
            avv(k,i,j) = avv(k,i,j) + v(k,i,j,np)*timeinc
#ifdef nflux_write
c--- Multiply northward heat flux by specific heat capacity (cm**3)
            avnflux(k,i,j,1) = avnflux(k,i,j,1) + nflux(k,i,j,1)
     &      *4.2*timeinc
c--- Divide northward FW flux by 1e6 (Sv)
            avnflux(k,i,j,2) = avnflux(k,i,j,2) + nflux(k,i,j,2)
     &      *0.000001*timeinc
#endif /*nflux_write*/
#ifdef ekin_write
c --- Specific kinetic energy of seawater in m^2/s^2
            ekin(k,i,j) = ekin(k,i,j) + ((u(k,i,j,np)*0.01)**2
     &      + (v(k,i,j,np)*0.01)**2)*timeinc
#endif /*ekin_write*/
#ifdef conv_write
            avConv(k,i,j) = avConv(k,i,j) + dble(conv(k,i,j))*timeinc
#endif /*conv_write*/
#ifdef c_write
            avcs(k,i,j) = avcs(k,i,j) + cs(k,i,j)*timeinc
#endif /*c_write*/
#ifdef rho_is_write
            rho_av(k,i,j) = rho_av(k,i,j) + rho_is(k,i,j)*timeinc
#endif /*rho_is_write*/
          enddo
#ifdef w_write
        do k=0,km
          avw(k,i,j) = avw(k,i,j) + w(k,i,j)*timeinc
        enddo
#endif /*w_write*/

#ifdef flux_write
          do it=1,nt
            avstf(i,j,it) = avstf(i,j,it)+surf_t_flux(i,j,it)*timeinc
            avsmf(i,j,it) = avsmf(i,j,it)+surf_m_flux(i,j,it)*timeinc
            avsnf(i,j,it) = avsnf(i,j,it)+surf_n_flux(i,j,it)*timeinc
          enddo
#endif /*flux_write*/

#ifdef ice_write
        if(avt(1,i,j,1).le.-1.8)
        avice(i,j) = 100
        else
        avice(i,j) = 0
        endif
#endif /*ice_write*/
        avu0(i,j) = avu0(i,j) + u0(i,j,np0)*timeinc
        avv0(i,j) = avv0(i,j) + v0(i,j,np0)*timeinc
        avh0(i,j) = avh0(i,j) + h0(i,j,np0)*timeinc

        enddo
      enddo
C$OMP END PARALLEL DO

      avtime = avtime + timeinc
      end

c     This routine normalises the time averaged fields...
      subroutine av_normalise
      implicit none
#include "param.h"
#include "average.h"
c local:
      integer i,j,k,it
      _MOMA_REAL norm

c     Use inverse of time and multiply since this is faster than division
      norm = c1/avtime

C$OMP PARALLEL DO PRIVATE( i , j ) , SCHEDULE(STATIC)
      do j=1,jmt
        do i=1,imt

          do it=1,nt
            do k=1,km
              avt(k,i,j,it) = avt(k,i,j,it)*norm
#ifdef tendency_write
              tend_adv_av(k,i,j,it) = tend_adv_av(k,i,j,it)*norm
              tend_hdiff_av(k,i,j,it) = tend_hdiff_av(k,i,j,it)*norm
              tend_vdiff_av(k,i,j,it) = tend_vdiff_av(k,i,j,it)*norm
#endif /*tendency_write*/
#ifdef nflux_write
            avnflux(k,i,j,it) = avnflux(k,i,j,it)*norm
#endif /*nflux_write*/
            enddo
            avstf(i,j,it) = avstf(i,j,it)*norm
            avsmf(i,j,it) = avsmf(i,j,it)*norm
            avsnf(i,j,it) = avsnf(i,j,it)*norm
          enddo

          do k=1,km
            avu(k,i,j) = avu(k,i,j)*norm
            avv(k,i,j) = avv(k,i,j)*norm
#ifdef ekin_write
            ekin(k,i,j) = ekin(k,i,j)*norm
#endif /*ekin_write*/
#ifdef rho_is_write
            rho_av(k,i,j) = rho_av(k,i,j)*norm
#endif /*rho_is_write*/
#ifdef conv_write
            avConv(k,i,j) = avConv(k,i,j)*norm
#endif /*conv_write*/
#ifdef c_write
            avcs(k,i,j) = avcs(k,i,j)*norm
#endif /*c_write*/
          enddo
#ifdef w_write
          do k=0,km
            avw(k,i,j) = avw(k,i,j)*norm
          enddo
#endif /*w_write*/
          avu0(i,j) = avu0(i,j)*norm
          avv0(i,j) = avv0(i,j)*norm
          avh0(i,j) = avh0(i,j)*norm
        enddo
      enddo
C$OMP END PARALLEL DO

c This prevents normalising more than once having any effect
      avtime = c1
      end

c
c -------------------------------------------------------------------
c
#ifdef netcdf
      subroutine snap_avg_netcdf
      implicit none
#include "param.h"
#include "ctmngr.h"
#include "netcdf_moma.h"
#include "timelv.h"
#include "average.h"

c     local:
      character path*150
      logical around
c
c     This small common block stores the filename and time level index
c     to be used when many snap shots are written to a single file...
      integer idft_snap
      character netcdffile*200
      common / com_netcdf_avsnap_i / idft_snap
      common / com_netcdf_avsnap_c / netcdffile

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
#endif /*flux_write*/
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
       path='./'            
       netcdffile = path(1:lnblnk(path))//'d'//
     &      netcdffile(3:10)//'.avg.nc'
       inquire(file = netcdffile, exist = around )
       if(around) then
        write(stdout,*)'Warning - Clobbering old average file!'
       endif
c Kill old file and create new one
       call new_netcdf_file(netcdffile)
      endif

      call cgs_to_base(km,avt(1,1,1,1),0)
      call mnc_write_4d( netcdffile , avt(1,1,1,1) ,
     &     'theta0','DEPTH','LONGITUDE_T','LATITUDE_T','DAY',
     &     'sea_water_potential_temperature',
     &     'K',REAL_TYPE,precision,fn_kmt3d)
      call base_to_cgs(km,avt(1,1,1,1),0)

      call cgs_to_base(km,avt(1,1,1,2),2)
      call mnc_write_4d( netcdffile , avt(1,1,1,2) ,
     &     'so','DEPTH','LONGITUDE_T','LATITUDE_T','DAY',
     &     'sea_water_salinity',
     &     '1e-3',REAL_TYPE,precision,fn_kmt3d)
      if(idft_snap.eq.1) then
      call base_to_cgs(km,avt(1,1,1,2),2)
c     Add special attributes for salinity if this is the first write.
c        call add_scale_off(netcdffile, 'so', 1000.d0,35.d0,
c     &       REAL_TYPE)
      endif


      call cgs_to_base(km,avu(1,1,1),1)
      call mnc_write_4d( netcdffile , avu(1,1,1) ,
     &     'uo','DEPTH','LONGITUDE_U','LATITUDE_U','DAY',
     &     'eastward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu3d)
      call base_to_cgs(km,avu(1,1,1),1)

      call cgs_to_base(km,avv(1,1,1),1)
      call mnc_write_4d( netcdffile , avv(1,1,1) ,
     &     'vo','DEPTH','LONGITUDE_U','LATITUDE_U','DAY',
     &     'northward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu3d)
      call base_to_cgs(km,avv(1,1,1),1)

#ifdef tendency_write
      call mnc_write_4d( netcdffile , tend_adv_av(1,1,1,1) ,
     &     't_tend_adv','DEPTH','LONGITUDE_U','LATITUDE_U','DAY',
     &     'tendency_of_sea_water_potential_temperature_due_
     &to_advection',
     &     'K/s',REAL_TYPE,precision,fn_kmt3d)

      call mnc_write_4d( netcdffile , tend_hdiff_av(1,1,1,1) ,
     &     't_tend_hdiff','DEPTH','LONGITUDE_U','LATITUDE_U','DAY',
     &     'tendency_of_sea_water_potential_temperature_due_
     &to_horizontal_diffusion',
     &     'K/s',REAL_TYPE,precision,fn_kmt3d)

      call mnc_write_4d( netcdffile , tend_vdiff_av(1,1,1,1) ,
     &     't_tend_vdiff','DEPTH','LONGITUDE_U','LATITUDE_U','DAY',
     &     'tendency_of_sea_water_potential_temperature_due_
     &to_vertical_diffusion',
     &     'K/s',REAL_TYPE,precision,fn_kmt3d)

      call cgs_to_base(km,tend_adv_av(1,1,1,2),3)
      call mnc_write_4d( netcdffile , tend_adv_av(1,1,1,2) ,
     &     's_tend_adv','DEPTH','LONGITUDE_U','LATITUDE_U','DAY',
     &     'tendency_of_sea_water_salinity_due_
     &to_advection',
     &     'psu/s',REAL_TYPE,precision,fn_kmt3d)
      call base_to_cgs(km,tend_adv_av(1,1,1,2),3)

      call cgs_to_base(km,tend_hdiff_av(1,1,1,2),3)
      call mnc_write_4d( netcdffile , tend_hdiff_av(1,1,1,2) ,
     &     's_tend_hdiff','DEPTH','LONGITUDE_U','LATITUDE_U','DAY',
     &     'tendency_of_sea_water_salinity_due_
     &to_horizontal_diffusion',
     &     'psu/s',REAL_TYPE,precision,fn_kmt3d)
      call base_to_cgs(km,tend_hdiff_av(1,1,1,2),3)

      call cgs_to_base(km,tend_vdiff_av(1,1,1,2),3)
      call mnc_write_4d( netcdffile , tend_vdiff_av(1,1,1,2) ,
     &     's_tend_vdiff','DEPTH','LONGITUDE_U','LATITUDE_U','DAY',
     &     'tendency_of_sea_water_salinity_due_
     &to_vertical_diffusion',
     &     'psu/s',REAL_TYPE,precision,fn_kmt3d)
      call base_to_cgs(km,tend_vdiff_av(1,1,1,2),3)
#endif /*tendency_write*/


#ifdef nflux_write
      call mnc_write_4d( netcdffile , avnflux(1,1,1,1) ,
     &     'nheat','DEPTH','LONGITUDE_U','LATITUDE_U','DAY',
     &     'northward_ocean_heat_transport',
     &     'W',REAL_TYPE,precision,fn_kmu3d)

      call mnc_write_4d( netcdffile , avnflux(1,1,1,2) ,
     &     'nfreshw','DEPTH','LONGITUDE_U','LATITUDE_U','DAY',
     &     'northward_ocean_freshwater_transport',
     &     'psu/s',REAL_TYPE,precision,fn_kmu3d)

#endif /*nflux_write*/

#ifdef ekin_write
      call mnc_write_4d( netcdffile , ekin(1,1,1) ,
     &     'ekin','DEPTH','LONGITUDE_U','LATITUDE_U','DAY',
     &     'specific_kinetic_energy_of_sea_water',
     &     'm^2/s^2',REAL_TYPE,precision,fn_kmu3d)
#endif /*ekin_write*/

#ifdef rho_is_write
      call mnc_write_4d( netcdffile , rho_av(1,1,1) ,
     &     'rho_is','DEPTH','LONGITUDE_T','LATITUDE_T','DAY',
     &     'sea_water_density',
     &     'kg/m^3',REAL_TYPE,precision,fn_kmt3d)
#endif /*rho_is_write*/

#ifdef w_write
      call cgs_to_base(kmp1,avw(0,1,1),1)
      call mnc_write_4d( netcdffile , avw(0,1,1) ,
     &     'wo','DEPTH_EDGES','LONGITUDE_T','LATITUDE_T','DAY',
     &     'upward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmt3d)
      call base_to_cgs(kmp1,avw(0,1,1),1)
#endif /*w_write*/

#ifdef c_write
      call mnc_write_4d( netcdffile , avcs(1,1,1) ,
     &     'co','DEPTH','LONGITUDE_T','LATITUDE_T','DAY',
     &     'speed_of_sound_in_sea_water',
     &     'm/s',REAL_TYPE,precision,fn_kmt3d)
#endif /*c_write*/

#ifdef ice_write
      call mnc_write_4d( netcdffile , avice(1,1) ,
     &     'sic','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'sea_ice_area_fraction',
     &     '%',REAL_TYPE,precision,fn_kmt3d)
#endif /*ice_write*/

c      call cgs_to_base(km,avu0(1,1),1)
      call mnc_write_4d( netcdffile , avu0(1,1) ,
     &     'buo','LONGITUDE_U','LATITUDE_U','DAY',' ',
     &     'barotropic_eastward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu2d)
c      call base_to_cgs(km,avu0(1,1),1)

      print*,'commented out buo cgs'

      call cgs_to_base(km,avv0(1,1),1)
      call mnc_write_4d( netcdffile , avv0(1,1) ,
     &     'bvo','LONGITUDE_U','LATITUDE_U','DAY',' ',
     &     'barotropic_northward_sea_water_velocity',
     &     'm/s',REAL_TYPE,precision,fn_kmu2d)
      call base_to_cgs(km,avv0(1,1),1)

      call cgs_to_base(km,avh0(1,1),1)
      call mnc_write_4d( netcdffile , avh0(1,1) ,
     &     'zos','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'sea_surface_height_above_geoid',
     &     'm',REAL_TYPE,precision,fn_kmt2d)
      call base_to_cgs(km,avh0(1,1),1)

#ifdef flux_write
      call mnc_write_4d( netcdffile , avstf(1,1,1) ,
     &     'hfo','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'surface_downward_heat_flux_into_sea_water',
     &     'W/m^2',REAL_TYPE,precision,fn_kmt2d)

c      call mnc_write_4d( netcdffile , avsnf(1,1,1) ,
c     &     'OasisTFLUX','LONGITUDE_T','LATITUDE_T','DAY',' ',
c     &     'Surface Temp. Flux',
c     &     'W/m^2',REAL_TYPE,precision,fn_kmt2d)

      call mnc_write_4d( netcdffile , avstf(1,1,2) ,
     &     'wfo','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'water_flux_into_ocean',
     &     'm/year',REAL_TYPE,precision,fn_kmt2d)

c      call mnc_write_4d( netcdffile , avsnf(1,1,2) ,
c     &     'OasisSFLUX','LONGITUDE_T','LATITUDE_T','DAY',' ',
c     &     'Surface Salinity. Flux',
c     &     'm/year',REAL_TYPE,precision,fn_kmt2d)

      call cgs_to_base(km,avsmf(1,1,1),4)
      call mnc_write_4d( netcdffile , avsmf(1,1,1) ,
     &     'tauu','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'surface_downward_eastward_stress',
     &     'Pa',REAL_TYPE,precision,fn_kmt2d)
      call base_to_cgs(km,avsmf(1,1,1),4)

      print*,'commented out tauu cgs'

      call cgs_to_base(km,avsmf(1,1,2),4)
      call mnc_write_4d( netcdffile , avsmf(1,1,2) ,
     &     'tauv','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'surface_downward_northward_stress',
     &     'Pa',REAL_TYPE,precision,fn_kmt2d)
      call base_to_cgs(km,avsmf(1,1,2),4)

      print*,'commented out tauv cgs'

      if(idft_snap.eq.1) then
c     Add scale factor attribute for fluxes if this is the first write.
        call add_scale_off(netcdffile, 'hfo', SCALE_STF1,c0,
     &       REAL_TYPE)
c        call add_scale_off(netcdffile, 'OasisTFLUX', SCALE_STF1,c0,
c     &       REAL_TYPE)
        call add_scale_off(netcdffile, 'wfo', SCALE_STF2,c0,
     &       REAL_TYPE)
c        call add_scale_off(netcdffile, 'OasisSFLUX', SCALE_STF2,c0,
c     &       REAL_TYPE)
      endif
#endif /*flux_write*/

#ifdef conv_write
      call mnc_write_4d( netcdffile , avconv(1,1,1) ,
     &     'CONV','DEPTH','LONGITUDE_T','LATITUDE_T','DAY',
     &     'Convective Mixing',
     &     'Number of times',REAL_TYPE,precision,fn_kmt3d)
#endif /*conv_write*/

      call mnc_write_4d( netcdffile , totsec ,
     &     'SECONDS','DAY',' ',' ',' ',
     &     'Elapsed model time',
     &     'seconds',REAL_TYPE,mnc_double,fn_true)

      call mnc_write_4d( netcdffile , years ,
     &     'YEAR','DAY',' ',' ',' ',
     &     'Elapsed model time',
     &     'years',REAL_TYPE,mnc_double,fn_true)

      end
#endif /*netcdf*/

#ifdef cmip_flx
c
c -------------------------------------------------------------------
c
#ifdef netcdf
      subroutine snap_flx_netcdf
      implicit none
#include "param.h"
#include "ctmngr.h"
#include "netcdf_moma.h"
#include "timelv.h"
#include "average.h"
#include "cvbc.h"

c     local:
      character path*150
      logical around
c
c     This small common block stores the filename and time level index
c     to be used when many snap shots are written to a single file...
      integer idft_snap
      character netcdffile*200
      common / com_netcdf_flxsnap_i / idft_snap
      common / com_netcdf_flxsnap_c / netcdffile

      integer precision
      parameter(precision = mnc_double)

      _MOMA_REAL SCALE_STF1,SCALE_STF2
      parameter(SCALE_STF1 = 4.184d4)
c     cal/cm^2/sec to W/m^2 into ocean =(cal/sec per Watt)*(cm^2 per m^2)
      parameter(SCALE_STF2 = -9.0102857143d6)
c     ppt/cm^2/sec to m/year of fresh water into ocean (P-E)
c      Assuming 1year=365.0 days & salinity of ocean = 35ppt.
c      Scale = (1000/35 ) * 86400 * 365 /100
c       = (1/salinity of ocean) * (secs per day) * (days per year) / (cm per m)
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
       path='./'  
       netcdffile = path(1:lnblnk(path))//'f'//
     &      netcdffile(3:10)//'.nc'
       inquire(file = netcdffile, exist = around )
       if(around) then
        write(stdout,*)'Warning - Clobbering old average file!'
       endif
c Kill old file and create new one
       call new_netcdf_file(netcdffile)
      endif

      call mnc_write_4d( netcdffile , surf_t_flux(1,1,1) ,
     &     'hfo','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'surface_downward_heat_flux_into_sea_water',
     &     'W/m^2',REAL_TYPE,precision,fn_kmt2d)
      call mnc_write_4d( netcdffile , surf_t_flux(1,1,2) ,
     &     'wfo','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'water_flux_into_ocean',
     &     'm/year',REAL_TYPE,precision,fn_kmt2d)
      if(idft_snap.eq.1) then
c     Add scale factor attribute for fluxes if this is the first write.
        call add_scale_off(netcdffile, 'hfo', SCALE_STF1,c0,
     &       REAL_TYPE)
        call add_scale_off(netcdffile, 'wfo', SCALE_STF2,c0,
     &       REAL_TYPE)
      endif

      call mnc_write_4d( netcdffile , totsec ,
     &     'SECONDS','DAY',' ',' ',' ',
     &     'Elapsed model time',
     &     'seconds',REAL_TYPE,mnc_double,fn_true)

      call mnc_write_4d( netcdffile , years ,
     &     'YEAR','DAY',' ',' ',' ',
     &     'Elapsed model time',
     &     'years',REAL_TYPE,mnc_double,fn_true)

      end

#endif /*netcdf*/
#endif /*cmip_flx*/

#ifdef cmip_flxrd
c
c -------------------------------------------------------------------
c
#ifdef netcdf
      subroutine snap_flxrd_netcdf
      implicit none
#include "param.h"
#include "ctmngr.h"
#include "netcdf_moma.h"
#include "timelv.h"
#include "average.h"
#include "cvbc.h"
#include "iounit.h"

c     local:
c     Whether any of the read fields mismatch the mask :
      logical lerr
      integer i,j,k

      character netcdffile*200

      integer precision

      lerr = .false.
      netcdffile=fnflx

      call mnc_read_4d( netcdffile , surfrd_t_flux(1,1,1) ,
     &     'hfo','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'surface_downward_heat_flux_into_sea_water',
     &     'W/m^2',REAL_TYPE,precision,fn_kmt2d)
      call mnc_read_4d( netcdffile , surfrd_t_flux(1,1,2) ,
     &     'wfo','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'water_flux_into_ocean',
     &     'm/year',REAL_TYPE,precision,fn_kmt2d)

      end

#endif /*netcdf*/
#endif /*cmip_flxrd*/

#ifdef sst_wr
c
c -------------------------------------------------------------------
c
#ifdef netcdf
      subroutine snap_sst_wr_netcdf
      implicit none
#include "param.h"
#include "ctmngr.h"
#include "netcdf_moma.h"
#include "timelv.h"
#include "average.h"
#include "cvbc.h"
#include "oasis.h"

c     local:
      character path*150
      logical around
c
c     This small common block stores the filename and time level index
c     to be used when many snap shots are written to a single file...
      integer idft_snap
      character netcdffile*200
      common / com_netcdf_sstsnap_i / idft_snap
      common / com_netcdf_sstsnap_c / netcdffile

      integer precision
      parameter(precision = mnc_double)

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
       netcdffile = path(1:lnblnk(path))//'s'//
     &      netcdffile(3:10)//'.nc'
       inquire(file = netcdffile, exist = around )
       if(around) then
        write(stdout,*)'Warning - Clobbering old average file!'
       endif
c Kill old file and create new one
       call new_netcdf_file(netcdffile)
      endif

      call mnc_write_4d( netcdffile , sstoc(1,1) ,
     &     'SST','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'sea_surface_temperature',
     &     'C',REAL_TYPE,precision,fn_kmt2d)

      call mnc_write_4d( netcdffile , totsec ,
     &     'SECONDS','DAY',' ',' ',' ',
     &     'Elapsed model time',
     &     'seconds',REAL_TYPE,mnc_double,fn_true)

      call mnc_write_4d( netcdffile , years ,
     &     'YEAR','DAY',' ',' ',' ',
     &     'Elapsed model time',
     &     'years',REAL_TYPE,mnc_double,fn_true)

      end

#endif /*netcdf*/
#endif /*sst_wr*/

#ifdef sst_rd
c
c -------------------------------------------------------------------
c
#ifdef netcdf
      subroutine snap_sst_rd_netcdf
      implicit none
#include "param.h"
#include "ctmngr.h"
#include "netcdf_moma.h"
#include "timelv.h"
#include "average.h"
#include "cvbc.h"
#include "iounit.h"
#include "oasis.h"

c     local:
c     Whether any of the read fields mismatch the mask :
      logical lerr
      integer i,j,k

      character netcdffile*200

      integer precision

      lerr = .false.
      netcdffile=fnsst

      call mnc_read_4d( netcdffile , sstoc(1,1) ,
     &     'SST','LONGITUDE_T','LATITUDE_T','DAY',' ',
     &     'sea_surface_temperature',
     &     'C',REAL_TYPE,precision,fn_kmt2d)

      end

#endif /*netcdf*/
#endif /*sst_rd*/



#endif /*average*/

