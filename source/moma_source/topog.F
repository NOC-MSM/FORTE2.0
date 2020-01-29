#include "cpp_options.h"
      subroutine topog
      implicit none
c
c-----------------------------------------------------------------------
c     construct an "idealized" world ... piece by piece
c     note: this topography will map into arbitrary model 
c     resolution
c-----------------------------------------------------------------------
c
#include "param.h"
#include "coord.h"
#include "grdvar.h"
#include "levind.h"
c Local:
      integer i,j,level,ic,jc

c
c     define the model topography on the "t" grid using "kmt"
c     set maximum level everywhere except on boundary
c
      do j=1,jmt
        do i=1,imt
          kmt(i,j) = 0
        end do
      end do
c
      do j=jcbeg,jcend
        do i=icbeg,icend
          kmt(i,j) = km
        end do
      end do
c
c     antarctica
c
      call setkmt (-90.0, 0.0, c360, -80.0, 0.0, c360, 0)
      call setkmt (-80.0, c360-25.0, c360, -70.0, c360, c360, 0)
      call setkmt (-80.0, 0.0, c360, -70.0, 0.0, 170.0, 0)
      call setkmt (-80.0, c360-135.0, c360-60.0, -68.0, c360-75.0,
     &              c360-60.0, 0)
      call setkmt (-70.0, 0.0, 155.0, -67.0, 50.0, 145.0, 0)
c
c     australia
c
      call setkmt (-35.0, 116.0, 120.0, -31.0, 114.0, 130.0, 0)
      call setkmt (-38.0, 140.0, 151.0, -31.0, 130.0, 151.0, 0)
      call setkmt (-31.0, 115.0, 153.0, -20.0, 113.0, 149.0, 0)
      call setkmt (-20.0, 113.0, 149.0, -11.0, 131.0, 143.0, 0)
c
c     south america
c
      call setkmt (-50.0, c360-74.0, c360-68.0, -40.0, c360-73.0,
     &              c360-62.0, 0)
      call setkmt (-40.0, c360-73.0, c360-62.0, -20.0, c360-70.0,
     &              c360-40.0, 0)
      call setkmt (-20.0, c360-70.0, c360-40.0, -16.0, c360-81.0,
     &              c360-35.0, 0)
      call setkmt (-16.0, c360-81.0, c360-35.0, 0.0, c360-80.0,
     &             c360-50.0, 0)
      call setkmt (0.0, c360-80.0, c360-50.0, 11.0, c360-75.0,
     &             c360-60.0, 0)
c
c     central america
c
      call setkmt (6.0, c360-78.0, c360-75.0, 20.0, c360-105.0,
     &             c360-97.0, 0)
      call setkmt (20.0, c360-105.0, c360-97.0, 30.0, c360-115.0,
     &             c360-94.0, 0)
c
c     north america
c
      call setkmt (25.0, c360-82.0, c360-80.0, 30.0, c360-85.0,
     &             c360-81.0, 0)
      call setkmt (30.0, c360-115.0, c360-80.0, 40.0, c360-124.0,
     &             c360-74.0, 0)
      call setkmt (40.0, c360-124.0, c360-74.0, 50.0, c360-124.0,
     &             c360-57.0, 0)
      call setkmt (50.0, c360-124.0, c360-57.0, 60.0, c360-140.0,
     &             c360-64.0, 0)
      call setkmt (60.0, c360-165.0, c360-64.0, 65.0, c360-140.0,
     &             c360-64.0, 0)
      call setkmt (65.0, c360-140.0, c360-64.0, 70.0, c360-162.0,
     &             c360-72.0, 0)
      call setkmt (70.0, c360-162.0, c360-140.0, 72.0, c360-157.0,
     &             c360-157.0, 0)
      call setkmt (70.0, c360-130.0, c360-70.0, 75.0, c360-120.0,
     &             c360-80.0, 0)
c
c     greenland
c
      call setkmt (60.0, c360-45.0, c360-45.0, 75.0, c360-58.0,
     &             c360-19.0, 0)
c
c     africa
c
      call setkmt (-35.0, 19.0, 28.0, 6.0, 8.0, 50.0, 0)
      call setkmt (6.0, 0.0, 50.0, 18.0, 0.0, 56.0, 0)
      call setkmt (18.0, 0.0, 56.0, 26.0, 0.0, 59.0, 0)
      call setkmt (6.0, c360-10.0, c360, 18.0, c360-18.0, c360, 0)
      call setkmt (18.0, c360-18.0, c360, 26.0, c360-15.0, c360, 0)
c
c     northern africa &  europe & asia
c
      call setkmt (26.0, c360-15.0, c360, 40.0, c360-7.0, c360, 0)
      call setkmt (40.0, c360-7.0, c360, 50.0, c360, c360, 0)
c
      call setkmt (26.0, 0.0, 126.0, 40.0, 0.0, 122.0, 0)
      call setkmt (40.0, 0.0, 130.0, 50.0, 0.0, 140.0, 0)
      call setkmt (50.0, 0.0, 140.0, 60.0, 8.0, 140.0, 0)
      call setkmt (60.0, 8.0, 163.0, 65.0, 13.0, 180.0, 0)
      call setkmt (65.0, 13.0, 188.0, 70.0, 20.0, 180.0, 0)
      call setkmt (70.0, 70.0, 180.0, 75.0, 90.0, 100.0, 0)
c
c     add an "idealized" ridge in the atlantic & pacific
c
      level = indp (3500.e2, zw, km)
c
      call setkmt (-20.0, c360-20.0, c360-10.0, 30.0, c360-45.0,
     &              c360-35.0, level)
      call setkmt (30.0, c360-45.0, c360-35.0, 60.0, c360-20.0,
     &              c360-30.0, level)
c
      call setkmt (-60.0,c360-100.0, c360-130.0, 40.0, c360-160.0,
     &              180.0, level)
      level = indp (2000.e2, zw, km)
      call setkmt (-50.0, c360-120.0, c360-120.0, 30.0, 190.0, 190.0,
     &             level)
c
c     set cyclic boundary conditions
c
      do jc=1,jmt
        do ic=1,nhalo
          kmt(ic,jc)        = kmt(imtm2h+ic,jc)
          kmt(imtm1h+ic,jc) = kmt(nhalo+ic,jc)
        end do
      end do
      return
      end
