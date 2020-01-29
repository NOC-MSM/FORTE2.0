      subroutine writegrids
      include "parameters.h"
      parameter(nxa=96,nya=48)
      parameter(nxa2=128,nya2=64)
      real*8 bb(nxo,nyo),cc(nxa,nya)
      real*8 atopalon(nxo,nyo),aat31lon(nxa,nya)
      real*8 atopalat(nxo,nyo),aat31lat(nxa,nya)
      real*8 auopalon(nxo,nyo),auopalat(nxo,nyo)
      real*8 avopalon(nxo,nyo),avopalat(nxo,nyo)
      real*8 aat32lon(nxa2,nya2)
      real*8 aat32lat(nxa2,nya2)
      character*8 aa
      character*8 topalon,topalat,uopalon,uopalat,
     1   vopalon,vopalat,at31lon,at31lat
c topa.lon

      open(45,file='OUTPUT/grids',form='unformatted')
      open(71,file='working/GRIDS71',form='unformatted')
      open(72,file='working/GRIDS72',form='unformatted')

      topalon='topa.lon'
      write(45)topalon
      write(6,"(a8)")topalon
      do j=1,nyo
      do i=1,nxo
      atopalon(i,j)=xoffs+float(i-1)*dxo
      end do
      end do
      write(45)atopalon
c     write(61,"(e16.8)")((atopalon(i,j),i=1,nxo),j=1,nyo)
c topa.lat
      topalat='topa.lat'
      write(45)topalat
      write(6,"(a8)")topalat
      do j=1,nyo
      do i=1,nxo
      atopalat(i,j)=yoffs+float(j-1)*dyo
      end do
      end do
      write(45)atopalat
c     write(62,"(e16.8)")((atopalat(i,j),i=1,nxo),j=1,nyo)
c uopa.lon
      uopalon='uopa.lon'
      write(45)uopalon
      write(6,"(a8)")uopalon
      do j=1,nyo
      do i=1,nxo
      auopalon(i,j)=xoffs+float(i-1)*dxo
      end do
      end do
      write(45)auopalon
c     write(63,"(e16.8)")((auopalon(i,j),i=1,nxo),j=1,nyo)
c uopa.lat
      uopalat='uopa.lat'
      write(45)uopalat
      write(6,"(a8)")uopalat
      do j=1,nyo
      do i=1,nxo
      auopalat(i,j)=yoffs+float(j-1)*dyo
      end do
      end do
      write(45)auopalat
c     write(64,"(e16.8)")((auopalat(i,j),i=1,nxo),j=1,nyo)
c vopa.lon
      vopalon='vopa.lon'
      write(45)vopalon
      write(6,"(a8)")vopalon
      do j=1,nyo
      do i=1,nxo
      avopalon(i,j)=xoffs+float(i-1)*dxo
      end do
      end do
      write(45)avopalon
c     write(65,"(e16.8)")((avopalon(i,j),i=1,nxo),j=1,nyo)
c vopa.lat
      vopalat='vopa.lat'
      write(45)vopalat
      write(6,"(a8)")vopalat
      do j=1,nyo
      do i=1,nxo
      avopalat(i,j)=yoffs+float(j-1)*dyo
      end do
      end do
      write(45)avopalat
c     write(66,"(e16.8)")((avopalat(i,j),i=1,nxo),j=1,nyo)
c at31.lon
      at31lon='at31.lon'
      write(45)at31lon
      write(6,"(a8)")at31lon
      read(71)aat32lon
      write(45)aat32lon
c     write(67,"(e16.8)")((aat31lon(i,j),i=1,nxa),j=1,nya)
c at31.lat
      at31lat='at31.lat'
      write(45)at31lat
      write(6,"(a8)")at31lat
      read(72)aat32lat
      write(45)aat32lat
c     write(68,"(e16.8)")((aat31lat(i,j),i=1,nxa),j=1,nya)
      write(6,*)aat31lon(43,9),aat31lat(43,9)
      write(6,*)aat31lon(43,10),aat31lat(43,10)
      write(6,*)aat31lon(43,8),aat31lat(43,8)
      write(6,*)aat31lon(44,9),aat31lat(44,9)
      write(6,*)aat31lon(43,9),aat31lat(43,9)
      write(6,*) " ----- "
      write(6,*)atopalon(51,2),atopalat(51,2)
      write(6,*)atopalon(51,3),atopalat(51,3)
      write(6,*)atopalon(51,4),atopalat(51,4)
      write(6,*)atopalon(51,5),atopalat(51,5)
      write(6,*)atopalon(52,2),atopalat(52,2)
      write(6,*)atopalon(52,3),atopalat(52,3)
      write(6,*)atopalon(52,4),atopalat(52,4)
      write(6,*)atopalon(52,5),atopalat(52,5)
      write(6,*) " ----- "
      do j=1,20
      write(6,*)atopalon(j,17),aat31lon(j,9)
      end do

      close(45)
      close(71)
      close(72)
      return
      stop
      end
