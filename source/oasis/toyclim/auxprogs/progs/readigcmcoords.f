      subroutine readigcmcoords
c      calculates oasis mozaic interpolation
c      weights between igcm3 and moma grids
      parameter(jg=32,ig=128)
      include "parameters.h"
      parameter(nya=2*jg,nxa=ig)
      double precision rlat(nya),w(jg),rl2(jg)
      double precision rlon(nxa),w2(nya)
      double precision rlatn(nya),rlats(nya)
      double precision rlone(nxa),rlonw(nxa)
      double precision atopa(nxa,nya)
      double precision atopala(nxa,nya)
      double precision atopalo(nxa,nya)
      double precision rlato(nyo),rlono(nxo)
      double precision rlaton(nyo),rlatos(nyo)
      double precision rlonoe(nxo),rlonow(nxo)
      double precision atopao(nxo,nyo)
      double precision pi, re,h,h1
      double precision wt(48,nxa*nya)
      integer iadress(48,nxa*nya),iat32(nxa,nya),
     1    iat(nxo,nyo),kmt(nxo,nyo)
      character*8 cladress,clweight

      open(73,file='working/MASKS73')

      do i=1,nxo
      rlono(i)=xoffs+float(i-1)*dxo
      if(rlono(i).gt.360.)rlono(i)=rlono(i)-360.
      if(rlono(i).lt.0.)rlono(i)=rlono(i)+360.
      write(6,*)i,rlono(i)
      end do
      do j=1,nyo
      rlato(j)=yoffs+float(j-1)*dyo
      write(6,*) " rlato = ",j,rlato(j)
      end do
      open(1,file="working/igcm3mask.i1")
      do j=nya,1,-1
      read(1,"(128i1)")(iat32(i,j),i=1,nxa)
      end do
      close(1)
      open(71,file="working/igcm3.coords")
      do j=1,nya
      read(71,*)nn,rlat(j),rlatn(j),rlats(j)
      end do
      read(71,*)
      do i=1,nxa
      read(71,*)nn,rlon(i),rlonw(i),rlone(i)
      end do
      write(6,*) " lons "
      do i=1,nxa
      write(6,*)i,rlon(i),rlonw(i)
      end do
      write(6,*) " lats "
      do j=1,nya
      write(6,*)j,rlat(j),rlats(j)
      end do
c     ii=37
c     jj=16
      open(2,file="ocean.kmtc.arctic_ridge_dp_ds")
      read(2,15)string,inxo,inyo,ikmt
       do 20 j=nyo,1,-1
         read(2,16)(kmt(i,j),i=1,nxo)
 20    continue
      do jj=1,nyo
      do ii=1,nxo
      if(kmt(ii,jj).eq.0)then
      iat(ii,jj)=1
      else
      iat(ii,jj)=0
      endif
      write(73,*)iat(ii,jj)
      end do
      end do
  15  format(1x,a3,5x,6i10)
  16  format(1x,100i3)
      do jj=1,nyo
      do ii=1,nxo
c     write(76,*)iat(ii,jj)
      end do
      end do
      close(73)
      return
      stop
      end
