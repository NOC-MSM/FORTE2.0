      subroutine writeareas
      implicit double precision(a-h,o-z),integer(i-n)
      include "parameters.h"
      parameter(nxa=96,nya=48)
      parameter(nxa2=128,nya2=64)
      real*8 bb(nxo,nyo),cc(nxa,nya)
      real*8 aat32(nxa2,nya2)
      real*8 atopa(nxo,nyo),aat31(nxa,nya)
      real*8 auopa(nxo,nyo)
      real*8 avopa(nxo,nyo)
      real*8 atopalon(nxo,nyo),aat31lon(nxa,nya)
      real*8 atopalat(nxo,nyo),aat31lat(nxa,nya)
      character*8 aa
      character*8 topa,uopa,
     $   vopa,at31
      pi=3.1415926535898d0
      re=6370d3

      open(45,file='OUTPUT/areas',form='unformatted')
      open(70,file='working/AREAS70',form='unformatted')

      do j=1,nyo
      do i=1,nxo
      atopalon(i,j)=xoffs+float(i-1)*dxo
      end do
      end do
      do j=1,nyo
      do i=1,nxo
      atopalat(i,j)=yoffs+float(j-1)*dyo
      end do
      end do
c topa
      topa='topa.srf'
      write(45)topa
      write(6,"(a8)")topa
      do is=1,nxo
      do js=1,nyo
      if(is.eq.1 .and. js.eq.1)then

      rlatn=.5d0*(atopalat(is,js+1)+atopalat(is,js))
      delta=rlatn-atopalat(is,js)
      rlats=atopalat(is,js)-delta
      rlone=.5d0*(atopalon(is+1,js)+atopalon(is,js))
      delta=rlone-atopalon(is,js)
      rlonw=atopalon(is,js)-delta

      else if(is.eq.1 .and. js.eq.nyo)then

      rlats=.5d0*(atopalat(is,js-1)+atopalat(is,js))
      delta=atopalat(is,js)-rlats
      rlatn=atopalat(is,js)+delta
      rlone=.5d0*(atopalon(is+1,js)+atopalon(is,js))
      delta=rlone-atopalon(is,js)
      rlonw=atopalon(is,js)-delta

      else if(is.eq.nxo .and. js.eq.1)then

      rlatn=.5d0*(atopalat(is,js+1)+atopalat(is,js))
      delta=rlatn-atopalat(is,js)
      rlats=atopalat(is,js)-delta
      rlonw=.5d0*(atopalon(is-1,js)+atopalon(is,js))
      delta=atopalon(is,js)-rlonw
      rlone=atopalon(is,js)+delta

      else if(is.eq.nxo .and. js.eq.nyo)then

      rlats=.5d0*(atopalat(is,js-1)+atopalat(is,js))
      delta=atopalat(is,js)-rlats
      rlatn=atopalat(is,js)+delta
      rlonw=.5d0*(atopalon(is-1,js)+atopalon(is,js))
      delta=atopalon(is,js)-rlonw
      rlone=atopalon(is,js)+delta

      else if(is.eq.1 .and. js.ne.1 .and. js.ne.nyo)then

      rlatn=.5d0*(atopalat(is,js+1)+atopalat(is,js))
      rlats=.5d0*(atopalat(is,js-1)+atopalat(is,js))
      rlone=.5d0*(atopalon(is+1,js)+atopalon(is,js))
      delta=rlone-atopalon(is,js)
      rlonw=atopalon(is,js)-delta

      else if(is.eq.nxo .and. js.ne.1 .and. js.ne.nyo)then

      rlatn=.5d0*(atopalat(is,js+1)+atopalat(is,js))
      rlats=.5d0*(atopalat(is,js-1)+atopalat(is,js))
      rlonw=.5d0*(atopalon(is-1,js)+atopalon(is,js))
      delta=atopalon(is,js)-rlonw
      rlone=atopalon(is,js)+delta

      else if(js.eq.1 .and. is.ne.1 .and. is.ne.nxo)then

      rlatn=.5d0*(atopalat(is,js+1)+atopalat(is,js))
      delta=rlatn-atopalat(is,js)
      rlats=atopalat(is,js)-delta
      rlone=.5d0*(atopalon(is+1,js)+atopalon(is,js))
      rlonw=.5d0*(atopalon(is-1,js)+atopalon(is,js))

      else if(js.eq.nyo .and. is.ne.1 .and. is.ne.nxo)then

      rlats=.5d0*(atopalat(is,js-1)+atopalat(is,js))
      delta=atopalat(is,js)-rlats
      rlatn=atopalat(is,js)+delta
      rlone=.5d0*(atopalon(is+1,js)+atopalon(is,js))
      rlonw=.5d0*(atopalon(is-1,js)+atopalon(is,js))

      else

      rlatn=.5d0*(atopalat(is,js+1)+atopalat(is,js))
      rlats=.5d0*(atopalat(is,js-1)+atopalat(is,js))
      rlone=.5d0*(atopalon(is+1,js)+atopalon(is,js))
      rlonw=.5d0*(atopalon(is-1,js)+atopalon(is,js))

      endif
      dlat=rlatn-rlats
      dlon=rlone-rlonw
      h=re*(dsin(pi*rlats/180.d0)-dsin(pi*rlatn/180.d0))
      h1=re*(dsin(pi*90.d0/180.d0)-dsin(pi*89.d0/180.d0))
      atopa(is,js)=pi*(2.d0*re*h)*dlon/360.d0
      if(atopa(is,js).lt.0.)atopa(is,js)=-atopa(is,js)
      if(is.eq.39 .and. js.eq.37)then
c     if(js.eq.37 .and. atopa(is,js).gt.1.e10)then
      write(6,*) " is = ",is
      write(6,*) " js = ",js
      write(6,*) " rlonw = ",rlonw
      write(6,*) " rlone = ",rlone
      write(6,*) " rlatn = ",rlatn
      write(6,*) " rlats = ",rlats
      write(6,*) " dlat = ",dlat
      write(6,*) " dlon = ",dlon
      write(6,*) " atopalat(is,js) = ",atopalat(is,js)
      write(6,*) " atopalat(is,js+1) = ",atopalat(is,js+1)
      write(6,*) " atopalat(is,js-1) = ",atopalat(is,js-1)
      write(6,*) " atopalon(is,js) = ",atopalon(is,js)
      write(6,*) " atopalon(is+1,js) = ",atopalon(is+1,js)
      write(6,*) " atopalon(is-1,js) = ",atopalon(is-1,js)
      write(6,*) " atopa(is,js) = ",atopa(is,js),h,h1
      endif
      end do
      end do
      write(45)atopa
c     write(61,"(e16.8)")((atopa(i,j),i=1,nxo),j=1,nyo)
c uopa
      uopa='uopa.srf'
      write(45)uopa
      write(6,"(a8)")uopa
      write(45)atopa
c     write(63,"(e16.8)")((atopa(i,j),i=1,nxo),j=1,nyo)
c vopa
      vopa='vopa.srf'
      write(45)vopa
      write(6,"(a8)")vopa
      write(45)atopa
c     write(65,"(e16.8)")((atopa(i,j),i=1,nxo),j=1,nyo)
c at31
      at31='at31.srf'
      write(45)at31
      write(6,"(a8)")at31
      do i=1,nxa2
      do j=1,nya2
      aat32(i,j)=aat31(i,j)
      end do
      end do
      read(70)aat32
      write(45)aat32
c     write(67,"(e16.8)")((aat31(i,j),i=1,nxa),j=1,nya)
      write(6,*)aat31(85,9)
      write(6,*)atopa(51,2)
      write(6,*)atopa(51,3)
      write(6,*)atopa(51,4)
      write(6,*)atopa(51,5)
      write(6,*)atopa(52,2)
      write(6,*)atopa(52,3)
      write(6,*)atopa(52,4)
      write(6,*)atopa(52,5)

      close(45)
      close(70)
      return
      stop
      end
