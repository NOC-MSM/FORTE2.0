#include "cpp_options.h"
      subroutine bcest (stf, smf, i, j)
      implicit none
c
c=======================================================================
c     this routine linearly interpolates global, zonal mean values 
c     of ocean model surface boundary conditions (SST, salinity, 
c     WSX, WSY) having 4.5 deg spacing, to the requested latitdude.
c=======================================================================
#include "param.h"
#include "coord.h"
c Arguments:
      _MOMA_REAL stf(nt), smf(2)
      integer i,j
c Local:
      integer nolat,nolatp
      parameter (nolat = 40, nolatp = nolat+1)
      _MOMA_REAL sstobs(nolat), salobs(nolat)
     & ,wsxobs(nolatp), wsyobs(nolatp)
     & , olatt(nolat), olatv(nolatp)
c
c     units of boundary condition values are:
c       degrees C, parts per thousand, dynes per square centimeter
c     olatt  = latitude points for observed data
c     olatv  = latitude points for observed data
c     dolat  = latitude spacing for observed data
c
c     "observed" temperature and salinity data are based on global,
c     annual mean zonally averaged values from the Levitus Atlas
c     (1982).
c     "observed" windstress data are based on global, annual mean,
c     zonally averaged values from Hellerman and Rosenstein (1981).
c     some smoothing was done.
c
c     units:  temperature - degrees Centigrade
c             salinity    - standard oceanographic salinity units
c             wind stress - dynes per sq cm
c     The salinity is transformed into model units in routine
c     'setvbc'.
c
c     references:
c       Hellerman, S, and M. Rosenstein, normal monthly wind stress
c         over the world ocean with error estimates, Journal of
c         Physical Oceanography, 13, 1093-1104,1983.
c       Levitus, S., Climatological atlas of the world ocean, NOAA
c         Prof. Paper 13, US Gov't printing Office, Washington, DC, 
c         1982.
c
c     'in' has been promoted to an array because it forces it to remain
c     threadprivate for OMP.
c
      integer in,n,nn,jj
      _MOMA_REAL ylatv,d,ylatt
      _MOMA_REAL dolat,rdolat
      common /bcest_comi/ in
      common /bcest_comr/ dolat,rdolat,olatt,olatv,sstobs, salobs
     &     ,wsxobs, wsyobs
C$OMP THREADPRIVATE( /bcest_comi/, /bcest_comr/ )

c
      data sstobs / -1.75, -1.75, -1.50, -1.50, -1.28,
     &              -0.55,  0.90,  2.92,  5.45,  8.62,
     &              12.27, 15.49, 18.30, 20.67, 22.64,
     &              24.14, 25.27, 26.37, 26.52, 26.16,
     &              26.85, 27.27, 26.82, 26.42, 25.53,
     &              24.03, 22.07, 19.73, 17.02, 12.77,
     &               8.93,  7.25,  6.22,  4.67,  4.57,
     &               3.03, -0.01, -1.05, -1.75, -1.75/
c
      data salobs / 34.30, 34.30, 34.30, 34.13, 33.98,
     &              33.97, 33.97, 33.98, 34.03, 34.24,
     &              34.61, 35.02, 35.37, 35.61, 35.72,
     &              35.68, 35.51, 35.22, 35.05, 35.12,
     &              34.80, 34.56, 34.71, 34.90, 35.27,
     &              35.67, 35.56, 35.49, 35.23, 34.28,
     &              33.57, 33.57, 33.60, 33.80, 34.04,
     &              34.05, 32.65, 32.30, 32.10, 32.00/
c
      data wsxobs /  0.00,
     &               0.00,  0.00, -0.02,  0.15,  0.31,
     &               0.50,  0.82,  1.08,  1.23,  1.16,
     &               0.84,  0.41,  0.02, -0.35, -0.55,
     &              -0.67, -0.64, -0.46, -0.29, -0.19,
     &              -0.16, -0.33, -0.52, -0.59, -0.55,
     &              -0.32,  0.09,  0.42,  0.56,  0.76,
     &               0.81,  0.65,  0.29,  0.06, -0.10,
     &              -0.05, -0.03,  0.05,  0.10,  0.01/
c
      data wsyobs /  .000,
     &               .000,  .009,  .032,  .005, -.023,
     &              -.075, -.155, -.202, -.230, -.179,
     &              -.049,  .093,  .214,  .294,  .344,
     &               .383,  .364,  .269,  .189,  .178,
     &               .125, -.122, -.213, -.251, -.259,
     &              -.202, -.189, -.179, -.183, -.009,
     &               .023,  .053, -.048, -.185, -.225,
     &              -.097, -.050, -.023, -.006,  .000/
c
      data in/1/
c
c----------------------------------------------------------------
c     set latitudes of sst and salinity observations
c     and set latitudes of windstress observations
c----------------------------------------------------------------
c
      if(in.eq.1) then
        in = -1
        dolat = 180.0/nolat
        rdolat = c1/dolat
        do 100 n=1,nolat
          olatt(n) = -90.0 + (n-p5)*dolat
          olatv(n) = -90.0 + (n-1.0)*dolat
100     continue
        olatv(nolatp) = -90.0 + (nolat)*dolat
      endif
c***************************************************************************************************
c----------------------------------------------------------------
c   use linear interpolation to produce the estimated surface 
c   boundary condition values for temperature and salinity at t,s 
c   row j
c----------------------------------------------------------------
c
c
    
      ylatt = yt(j)
      if (ylatt .le. olatt(1)) then
        nn = 1
        d = c0
      elseif (ylatt .ge. olatt(nolat)) then
        nn = nolat-1
        d = dolat
      else
        do 200 jj=2,nolat
          if (ylatt .le. olatt(jj)) then
            nn = jj - 1
            d  = ylatt - olatt(nn)
            goto 201
          endif
200     continue
      endif
201   continue
      stf(1) = (sstobs(nn)*(dolat - d) + sstobs(nn+1)*d)*rdolat
      stf(2) = (salobs(nn)*(dolat - d) + salobs(nn+1)*d)*rdolat
c
c----------------------------------------------------------------
c   use linear interpolation to produce the estimated surface 
c   boundary condition values for wind stress components at 
c   u,v row j
c----------------------------------------------------------------
c
      ylatv = yu(j)
c
      if (ylatv .le. olatv(1)) then
        nn = 1
        d = c0
      elseif (ylatv .ge. olatv(nolatp)) then
        nn = nolatp - 1
        d = dolat
      else
        do 300 jj=2,nolatp
          if (ylatv .le. olatv(jj)) then
            nn = jj - 1
            d  = ylatv - olatv(nn)
            goto 301
          endif
300     continue
      endif
301   continue
      smf(1) = (wsxobs(nn)*(dolat - d) + wsxobs(nn+1)*d) *rdolat
      smf(2) = (wsyobs(nn)*(dolat - d) + wsyobs(nn+1)*d) *rdolat

      return
      end
