
      parameter(mg=128,jg=32,mgpp=mg+2,igc=mgpp*2)
  
c      input arrays
      real*8 gsg(igc,jg)
      real*8 svege(260,32)
      real*8 svege2(260,32)

      open(59,file='OUTPUT/ofrac.igcm3')                 

      do j=1,jg
      read(59,*)(gsg(i,j),i=1,mg)
      end do
      do j=jg,1,-1
      read(59,*)(gsg(i+mgpp,j),i=1,mg)
      end do

      close(59)


      write(6,*) 'land mask looks like:'
      do j=1,jg
         write (6,'(40i2)')(nint( 10.*gsg(i,j)  ) ,i=1,40)
      enddo
      do j=jg,1,-1
         write (6,'(40i2)')(nint( 10.*gsg(i+mgpp,j) ) ,i=1,40)
      enddo

      do j=1,jg
         write (6,'(40i2)')(nint( 10.*gsg(i,j)  ) ,i=41,80)
      enddo
      do j=jg,1,-1
         write (6,'(40i2)')(nint( 10.*gsg(i+mgpp,j) ) ,i=41,80)
      enddo

      do j=1,jg
         write (6,'(40i2)')(nint( 10.*gsg(i,j)  ) ,i=81,120)
      enddo
      do j=jg,1,-1
         write (6,'(40i2)')(nint( 10.*gsg(i+mgpp,j) ) ,i=81,120)
      enddo

      do j=1,jg
         write (6,'(40i2)')(nint( 10.*gsg(i,j)  ) ,i=121,130)
      enddo
      do j=jg,1,-1
         write (6,'(40i2)')(nint( 10.*gsg(i+mgpp,j) ) ,i=121,130)
      enddo
c-----------------------------

      read(31,*) svege

      write(6,*) " hello "
  
      call interp(gsg,svege,svege2)
 
c     do j=1,jg
c        write (19,'(e16.8)')( svege(i,j),i=1,mg)
c     enddo
c     do j=jg,1,-1
c        write (19,'(e16.8)')( svege(i+mgpp,j),i=1,mg)
c     enddo
      write(19,*)svege

      write(32) svege2
      write(20,*) svege2
      end

      subroutine interp(gsg,svege,svege2)

      parameter(mg=128,jg=32,mgpp=mg+2,igc=mgpp*2)
  
c      input arrays
      real*8 gsg(igc,32)
      real*8 svege(260,32)
c      intermediate calcn arrays
      real*8 gsga(128,64)
      real*8 svegea(128,64)
      real*8 svegenew(128,64)
c      output array
      real*8 svege2(260,32)

c      first make veg type of points which are purely
c      ocean according to ofrac equal to 1
      do i=1,igc
      do j=1,jg
      if(gsg(i,j).lt.1. )then
      else
      svege(i,j)=1
      endif
      end do
      end do
c      now in a position to alter vege types
c      at coastal points which equal 1 to 
c      something more appropriate
c      convert to an i-j array
c     write(32,*) svege
      do j=1,32
      do i=1,128
         svegea(i,2*jg+1-j)=svege(i,j)
         svegenew(i,2*jg+1-j)=svege(i,j)
         gsga(i,2*jg+1-j)=gsg(i,j)
      enddo
      enddo
      do j=1,32
      do i=1,128
         svegea(i,j)=svege(i+130,j)
         svegenew(i,j)=svege(i+130,j)
         gsga(i,j)=gsg(i+130,j)
      enddo
      enddo
c      do a distance weighted interpolation
      do i=1,128
      do j=1,64
c      first look for points where the fractional grid says
c      it's a coastal point, but svege says it's ocean
      if(gsga(i,j).lt.1. .and. svegea(i,j).eq.1)then
      sumnum=0.
      sumden=0.
c      once you've found such a point look out to a
c      radius of two gridpoints in all directions
      do ii=-2,2
      do jj=-2,2
c      don't include the gridpoint itself of course
      if(ii.ne.0 .and. jj.ne.0)then
      ip=i+ii
c      the grid is periodic in i direction so if you look
c      too far in one direction you come back on the other
c      side of the domain
      if(ip.lt.0)ip=ip+128
      ip=mod(ip,128)
      if(ip.eq.0)ip=128
      jp=j+jj
c      only include points which remain in the domain in the
c      j direction
      if(jp.ge.1 .and. jp.le.64)then
       if(svegea(ip,jp).gt.1.)then
        r=sqrt( real(ii)**2 + real(jj)**2 )
        sumnum=sumnum+svegea(ip,jp)*1./r**2
        sumden=sumden+1./r**2
       endif
      endif
      endif
      end do !ii
      end do !jj
c      update svegenew
      if(sumden.ne.0.)then
      svegenew(i,j)=real( nint(sumnum/sumden) )
      else
c      you are in a bad way if this happens
c     stop 'bummer'
      svegenew(i,j)=24.
      endif

      endif
      end do
      end do

      do j=1,32
      do i=1,128
      svege2(i,j)=svegenew(i,2*jg+1-j)
      end do
      end do
      do j=1,32
      do i=1,128
      svege2(130+i,j)=svegenew(i,j)
      end do
      end do
      return
      end
