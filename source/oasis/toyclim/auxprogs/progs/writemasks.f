      subroutine writemasks
      include "parameters.h"
      parameter(nxa=96,nya=48)
      parameter(nxa2=128,nya2=64)
      real*8 bb(nxo,nyo),cc(nxa,nya)
      integer itopa(nxo,nyo),iat31(nxa,nya)
      integer iuopa(nxo,nyo)
      integer ivopa(nxo,nyo)
      integer iat32(nxa2,nya2)
      character*8 aa
      character*8 topa,uopa,
     1   vopa,at31

      open(45,file='OUTPUT/masks',form='unformatted')
      open(73,file='working/MASKS73')
c topa
      topa='topa.msk'
      write(45)topa
      write(6,"(a8)")topa
      do j=1,nyo
      do i=1,nxo
      read(73,*)itopa(i,j)
      end do
      end do
      write(45)itopa
c     write(61,"(i2)")((itopa(i,j),i=1,nxo),j=1,nyo)
c uopa
      uopa='uopa.msk'
      write(45)uopa
      write(6,"(a8)")uopa
      do j=1,nyo
      do i=1,nxo
      iuopa(i,j)=itopa(i,j)
      end do
      end do
      write(45)iuopa
c     write(63,"(i2)")((iuopa(i,j),i=1,nxo),j=1,nyo)
c vopa
      vopa='vopa.msk'
      write(45)vopa
      write(6,"(a8)")vopa
      do j=1,nyo
      do i=1,nxo
      ivopa(i,j)=itopa(i,j)
      end do
      end do
      write(45)ivopa
c     write(65,"(i2)")((ivopa(i,j),i=1,nxo),j=1,nyo)
c at31
      at31='at31.msk'
      write(45)at31
      write(6,"(a8)")at31
      open(1,file="working/igcm3mask.i1")
      do j=nya2,1,-1
      read(1,"(128i1)")(iat32(i,j),i=1,nxa2)
      end do
      close(1)
      write(45)iat32
c     write(67,"(i2)")((iat31(i,j),i=1,nxa),j=1,nya)
      close(45)
      close(73)
      return
      stop
      end
