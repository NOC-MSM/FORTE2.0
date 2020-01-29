      subroutine mosaicweights
c      calculates oasis mozaic interpolation
c      weights between igcm3 and moma grids

      parameter(jg=32,ig=128)
      include "parameters.h"
      parameter(nya=2*jg,nxa=ig)
      double precision rlat(nya),w(jg),rl2(jg)
      double precision rlon(nxa),w2(nya),wn2(nya)
     $   ,wn3(nya+1)
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
      double precision wtnew(48,nxa*nya)
      double precision wt(48,nxa*nya)
      double precision wt1(48,nxo*nyo)
      double precision ofrac(nxa,nya)
      double precision gwt(nya)
      real*8 atmosint(nxa,nya)
      integer iadressnew(48,nxa*nya)
      integer iadress(48,nxa*nya)
      integer iadress1(48,nxo*nyo)
      integer iland(nxa,nya)
      integer ilando(nxo,nyo)
      character*8 cladress,clweight

      PI=3.14159265359

c-----gaussian lats
      do j=1,jg
      call gwtlt(sit,weight,j,jg)
         SISQ=SIT*SIT
         CSSQ=1.-SISQ
         SECSQ=1./CSSQ
         CS=SQRT(CSSQ)
         ALAT=ATAN(SIT/CS)*180.0/PI
         GWT(nya+1-j)=WEIGHT/REAL(2)
         GWT(j)=WEIGHT/REAL(2)
         rlat(nya+1-j)=alat
         rlat(j)=-alat
         rl2(jg+1-j)=alat
      end do
      ww=0.

      do j=1,jg
        j2=jg+1-j
        ww=ww+gwt(j2)
        if(ww.gt..5)ww=.5
        wn2(j+jg)=asin(2.*ww)*180./pi
c     write(6,*) " ww = ",j,j2,ww,wn2(j+jg)
      end do

      do j=1,jg
        wn2(j)=-wn2(2*jg+1-j)
c     write(6,*) " ww = ",j,wn2(j)
      end do

      do j=1,2*jg
        if(j.le.jg)then
          wn3(j)=wn2(j)
        else
          wn3(j+1)=wn2(j)
        endif
      end do

      wn3(jg+1)=0.
c     write(6,*) " --- "
c     write(6,*) gwt(1),wn2(1)
c     write(6,*) " --- "
      do j=1,2*jg
c       write(6,*)j,gwt(j),wn3(j)
      end do

      do j=1,nya
c       write(6,*)j,rlat(j)
      end do
c     write(6,*) " ----- "

      w(1)=2.d0*rl2(1)
      do j=2,jg
        w(j)=2.d0*(rl2(j)-rl2(j-1)-w(j-1)/2.)
      end do

      do j=1,jg
        w2(j)=w(jg+1-j)
        w2(j+jg)=w(j)
      end do

      do j=1,nya
c       write(6,*)j,rlat(J),w2(j),gwt(j)*180.d0
      end do
c     write(6,*) " ----- "

      do j=1,nya
        rlatn(j)=rlat(j)+w2(j)/2.d0
        rlats(j)=rlat(j)-w2(j)/2.d0
      end do

        open(69,file='working/igcm3.coords')
      do j=1,nya
        rlatn(j)=wn3(j+1)
        rlats(j)=wn3(j)
c       write(6,"(i4,3e16.8)")j,rlat(j),rlatn(J),rlats(j)
        write(69,"(i4,3e16.8)")j,rlat(j),rlatn(J),rlats(j)
      end do
c     write(6,*) " ----- "
      write(69,*) " ----- "

      dx=360.d0/real(nxa)
      do i=1,nxa
        rlon(i)=dx/2.d0+real(i-1)*dx
      end do
      do i=1,nxa
        rlonw(i)=real(i-1)*dx
        rlone(i)=real(i)*dx
c       write(6,"(i4,3e16.8)")i,rlon(i),rlonw(i),rlone(i)
        write(69,"(i4,3e16.8)")i,rlon(i),rlonw(i),rlone(i)
      end do
        close(69)

c      make 2d arrays of lat and long for atmosphere
c      model for making oasis "grids" file
      do i=1,nxa
      do j=1,nya
        atopala(i,j)=rlat(j)
        atopalo(i,j)=rlon(i)
      end do
      end do

      pi=3.1415926535898d0
      re=6370d3
c      calculate areas for the atmosphere model
      do j=1,nya
      do i=1,nxa
        dlon=rlone(i)-rlonw(i)
        h=re*(dsin(pi*rlats(j)/180.d0)-dsin(pi*rlatn(j)/180.d0))
        atopa(i,j)=pi*(2.d0*re*h)*dlon/360.d0
        if(atopa(i,j).lt.0.d0)atopa(i,j)=-atopa(i,j)
c       write(61,*)atopa(i,j)
      end do
      end do

      open(70,file='working/AREAS70',form='unformatted')
      open(71,file='working/GRIDS71',form='unformatted')
      open(72,file='working/GRIDS72',form='unformatted')
      write(70)atopa
      write(71)atopalo
      write(72)atopala
      close (70)
      close (71)
      close (72)

c      calculate areas for the ocean model

      do i=1,nxo
        rlono(i)=xoffs+float(i-1)*dxo
        rlonoe(i)=rlono(i)+dxo/2.d0
        rlonow(i)=rlono(i)-dxo/2.d0
      write(6,*) rlono(i),rlonow(i),rlonoe(i)
      end do

      do j=1,nyo
        rlato(j)=yoffs+float(j-1)*dyo
        rlaton(j)=rlato(j)+dyo/2.d0
        rlatos(j)=rlato(j)-dyo/2.d0
        write(6,*) rlato(j),rlaton(j),rlatos(j)
      end do

      do j=1,nyo
      do i=1,nxo
        dlon=rlonoe(i)-rlonow(i)
        h=re*(dsin(pi*rlatos(j)/180.d0)-dsin(pi*rlaton(j)/180.d0))
        h1=re*(dsin(pi*90.d0/180.d0)-dsin(pi*89.d0/180.d0))
        atopao(i,j)=pi*(2.d0*re*h)*dlon/360.d0
        if(atopao(i,j).lt.0.)atopao(i,j)=-atopao(i,j)
c       write(62,*)atopao(i,j),h
      end do
      end do


      DO is=1,nxa
      DO js=1,nya

      iada=(js-1)*nxa+is
      kk=0

      do i=1,nxo
      do j=1,nyo
        IF
     $ ( rlonow(i) .gt. rlonw(is) 
     $            .and. 
     $   rlonoe(i) .lt. rlone(is) )
     $  THEN
      if(is.eq.1 .and. js.eq.1)then
      write(6,*) " i = ",i," j = ",j, "A"
      write(6,*) " rlatos = ",rlatos(j)
      write(6,*) " rlaton = ",rlaton(j)
      write(6,*) " rlonow = ",rlonow(i)
      write(6,*) " rlonoe = ",rlonoe(i)
      endif
          rlow=rlonow(i)
          rloe=rlonoe(i)
          if
     $ ( rlatos(j) .gt. rlats(js) 
     $            .and. 
     $   rlaton(j) .lt. rlatn(js) )
     $    then
            rlan=rlaton(j)
            rlas=rlatos(j)
            dlon=rloe-rlow
            h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
            weight=pi*(2.d0*re*h)*dlon/360.d0
            if(weight.lt.0.d0)weight=-weight
            iad=(j-1)*nxo+i
            kk=kk+1
            wt(kk,iada)=weight/atopa(is,js)
            iadress(kk,iada)=iad
          else if
     $ ( rlatos(j) .gt. rlats(js) 
     $            .and.
     $   rlatos(j) .lt. rlatn(js) 
     $            .and.
     $   rlaton(j) .ge. rlatn(js) )
     $    then
            rlan=rlatn(js)
            rlas=rlatos(j)
            dlon=rloe-rlow
            h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
            weight=pi*(2.d0*re*h)*dlon/360.d0
            if(weight.lt.0.d0)weight=-weight
            iad=(j-1)*nxo+i
            kk=kk+1
            wt(kk,iada)=weight/atopa(is,js)
            iadress(kk,iada)=iad
          else if
     $ ( rlatos(j) .le. rlats(js) 
     $            .and.
     $   rlaton(j) .gt. rlats(js)  
     $            .and.
     $   rlaton(j) .lt. rlatn(js) )
     $    then
            rlan=rlaton(j)
            rlas=rlats(js)
            dlon=rloe-rlow
            h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
            weight=pi*(2.d0*re*h)*dlon/360.d0
            if(weight.lt.0.d0)weight=-weight
            iad=(j-1)*nxo+i
            kk=kk+1
            wt(kk,iada)=weight/atopa(is,js)
            iadress(kk,iada)=iad
          endif
c----
        ELSE IF
     $ ( rlonow(i) .gt. rlonw(is) 
     $            .and.
     $   rlonow(i) .lt. rlone(is) 
     $            .and.
     $   rlonoe(i) .ge. rlone(is) )
     $  THEN
c     if(is.eq.1 .and. js.eq.1)then
c     write(6,*) " i = ",i," j = ",j, "B"
c     write(6,*) " rlatos = ",rlatos(j)
c     write(6,*) " rlaton = ",rlaton(j)
c     write(6,*) " rlonow = ",rlonow(i)
c     write(6,*) " rlonoe = ",rlonoe(i)
c     endif
          rlow=rlonow(i)
          rloe=rlone(is)
          if
     $ ( rlatos(j) .gt. rlats(js) 
     $            .and. 
     $   rlaton(j) .lt. rlatn(js) )
     $    then
      if(is.eq.1 .and. js.eq.1)then
      write(6,*) " i = ",i," j = ",j, "B1"
      write(6,*) " rlatos = ",rlatos(j)
      write(6,*) " rlaton = ",rlaton(j)
      write(6,*) " rlonow = ",rlonow(i)
      write(6,*) " rlonoe = ",rlonoe(i)
      endif
            rlan=rlaton(j)
            rlas=rlatos(j)
            dlon=rloe-rlow
            h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
            weight=pi*(2.d0*re*h)*dlon/360.d0
            if(weight.lt.0.d0)weight=-weight
            iad=(j-1)*nxo+i
            kk=kk+1
            wt(kk,iada)=weight/atopa(is,js)
            iadress(kk,iada)=iad
          else if
     $ ( rlatos(j) .gt. rlats(js) 
     $            .and.
     $   rlatos(j) .lt. rlatn(js) 
     $            .and.
     $   rlaton(j) .ge. rlatn(js) )
     $    then
      if(is.eq.1 .and. js.eq.1)then
      write(6,*) " i = ",i," j = ",j, "B2"
      write(6,*) " rlatos = ",rlatos(j)
      write(6,*) " rlaton = ",rlaton(j)
      write(6,*) " rlonow = ",rlonow(i)
      write(6,*) " rlonoe = ",rlonoe(i)
      endif
            rlan=rlatn(js)
            rlas=rlatos(j)
            dlon=rloe-rlow
            h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
            weight=pi*(2.d0*re*h)*dlon/360.d0
            if(weight.lt.0.d0)weight=-weight
            iad=(j-1)*nxo+i
            kk=kk+1
            wt(kk,iada)=weight/atopa(is,js)
            iadress(kk,iada)=iad
          else if
     $ ( rlatos(j) .le. rlats(js) 
     $            .and.
     $   rlaton(j) .gt. rlats(js)  
     $            .and.
     $   rlaton(j) .lt. rlatn(js) )
     $    then
      if(is.eq.1 .and. js.eq.1)then
      write(6,*) " i = ",i," j = ",j, "B3"
      write(6,*) " rlatos = ",rlatos(j)
      write(6,*) " rlaton = ",rlaton(j)
      write(6,*) " rlonow = ",rlonow(i)
      write(6,*) " rlonoe = ",rlonoe(i)
      endif
            rlan=rlaton(j)
            rlas=rlats(js)
            dlon=rloe-rlow
            h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
            weight=pi*(2.d0*re*h)*dlon/360.d0
            if(weight.lt.0.d0)weight=-weight
            iad=(j-1)*nxo+i
            kk=kk+1
            wt(kk,iada)=weight/atopa(is,js)
            iadress(kk,iada)=iad
          endif
c----
      ELSE IF
     $ ( rlonow(i) .le. rlonw(is) 
     $            .and.
     $   rlonoe(i) .gt. rlonw(is) 
     $            .and.
     $   rlonoe(i) .lt. rlone(is) )
     $THEN
c     if(is.eq.1 .and. js.eq.1)then
c     write(6,*) " i = ",i," j = ",j, "C"
c     write(6,*) " rlatos = ",rlatos(j)
c     write(6,*) " rlaton = ",rlaton(j)
c     write(6,*) " rlonow = ",rlonow(i)
c     write(6,*) " rlonoe = ",rlonoe(i)
c     endif
      rlow=rlonw(is)
      rloe= rlonoe(i)
c     write(6,*) " Ci = ",i
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c----
      if
     $ ( rlatos(j) .gt. rlats(js) 
     $            .and. 
     $   rlaton(j) .lt. rlatn(js) )
     $ then
      if(is.eq.1 .and. js.eq.1)then
      write(6,*) " i = ",i," j = ",j, "C1"
      write(6,*) " rlatos = ",rlatos(j)
      write(6,*) " rlaton = ",rlaton(j)
      write(6,*) " rlonow = ",rlonow(i)
      write(6,*) " rlonoe = ",rlonoe(i)
      endif
      rlan=rlaton(j)
      rlas=rlatos(j)
      dlon=rloe-rlow
      h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
      weight=pi*(2.d0*re*h)*dlon/360.d0
      if(weight.lt.0.d0)weight=-weight
      iad=(j-1)*nxo+i
      kk=kk+1
      wt(kk,iada)=weight/atopa(is,js)
      iadress(kk,iada)=iad
c     write(6,*)" CA ",i,j,iad,weight,kk
c     write(6,*) " CAj = ",j
c     write(6,*) " CAi = ",i
c     write(6,*) " rlan = ",rlan
c     write(6,*) " rlas = ",rlas
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c     write(6,*) " ------- "
      else if
     $ ( rlatos(j) .gt. rlats(js) 
     $            .and.
     $   rlatos(j) .lt. rlatn(js) 
     $            .and.
     $   rlaton(j) .ge. rlatn(js) )
     $ then
      if(is.eq.1 .and. js.eq.1)then
      write(6,*) " i = ",i," j = ",j, "C2"
      write(6,*) " rlatos = ",rlatos(j)
      write(6,*) " rlaton = ",rlaton(j)
      write(6,*) " rlonow = ",rlonow(i)
      write(6,*) " rlonoe = ",rlonoe(i)
      endif
      rlan=rlatn(js)
      rlas=rlatos(j)
      dlon=rloe-rlow
      h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
      weight=pi*(2.d0*re*h)*dlon/360.d0
      if(weight.lt.0.d0)weight=-weight
      iad=(j-1)*nxo+i
      kk=kk+1
      wt(kk,iada)=weight/atopa(is,js)
      iadress(kk,iada)=iad
c     write(6,*)" CB ",i,j,iad,weight,kk
c     write(6,*) " CBj = ",j
c     write(6,*) " CBi = ",i
c     write(6,*) " rlan = ",rlan
c     write(6,*) " rlas = ",rlas
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c     write(6,*) " ------- "
      else if
     $ ( rlatos(j) .le. rlats(js) 
     $            .and.
     $   rlaton(j) .gt. rlats(js)  
     $            .and.
     $   rlaton(j) .lt. rlatn(js) )
     $ then
      if(is.eq.1 .and. js.eq.1)then
      write(6,*) " i = ",i," j = ",j, "C3"
      write(6,*) " rlatos = ",rlatos(j)
      write(6,*) " rlaton = ",rlaton(j)
      write(6,*) " rlonow = ",rlonow(i)
      write(6,*) " rlonoe = ",rlonoe(i)
      endif
      rlan=rlaton(j)
      rlas=rlats(js)
      dlon=rloe-rlow
      h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
      weight=pi*(2.d0*re*h)*dlon/360.d0
      if(weight.lt.0.d0)weight=-weight
      iad=(j-1)*nxo+i
      kk=kk+1
      wt(kk,iada)=weight/atopa(is,js)
      iadress(kk,iada)=iad
c     write(6,*)" CC ",i,j,iad,weight,kk
c     write(6,*) " CCj = ",j
c     write(6,*) " CCi = ",i
c     write(6,*) " rlan = ",rlan
c     write(6,*) " rlas = ",rlas
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c     write(6,*) " ------- "
c     write(6,*) " rlatos = ",rlatos(j)
c     write(6,*) " rlaton = ",rlaton(j)
c     write(6,*) " rlonow = ",rlonow(i)
c     write(6,*) " rlonoe = ",rlonoe(i)
      endif
c----
      END IF
      end do
      end do
c     write(6,*) " iada = ",iada
c     summ=0.d0
c     do k=1,48
c     summ=summ+wt(k,iada)
c     write(6,*)k, wt(k,iada),iadress(k,iada)
c     end do
c     write(6,*) " summ = ",summ

      if(is.eq.1 .and. js.eq.1)then
        write(6,*)" hellow weights ",kk
      do k6=1,kk
      write(6,*)k6,wt(k6,iada)
      end do
      write(6,*) " N-S ",rlats(js),rlatn(js)
      write(6,*) " E-W ",rlonw(is),rlone(is)
      write(6,*) " rlan = ",rlan
      write(6,*) " rlas = ",rlas
      write(6,*) " rlow = ",rlow
      write(6,*) " rloe = ",rloe
      endif

      END DO
      END DO
c-----------------
c      read in the igcm land mask
      open(1,file="working/igcm3mask.i1")
      do j=nya,1,-1
      read(1,"(128i1)")(iland(i,j),i=1,nxa)
      end do
      close(1)
      if(mod(itest,100).eq.0)write(6,*)itest
c      read in the ocean land mask
      open(1,file="ocean.kmtc.arctic_ridge_dp_ds_mk2")
      read(1,*)
      do j=nyo,1,-1
      read(1,"(1x,100i3)")(ilando(i,j),i=1,nxo)
      end do
      close(1)
      do itest = 1,8192
c      convert the address to a i,j value
c      for the igcm
      if(mod(itest,ig).eq.0)then
      jval=itest/ig
      ival=ig
      else
      jval=itest/ig+1
      ival=itest-(jval-1)*ig
      endif
c      go through the ocean adresses for this igcm
c      gridpoint, and work out the i,j values of
c      each contributing ocean gridpoint
      sumw=0.
      sumtot=0.
      nland=0
      nsea=0
      do ii=1,48
      if(wt(ii,itest).gt.0.)then
      sumtot=sumtot+wt(ii,itest)
      if(mod(iadress(ii,itest),nxo).eq.0)then
      jvalo=iadress(ii,itest)/nxo
      ivalo=nxo
      else
      jvalo=iadress(ii,itest)/nxo+1
      ivalo=iadress(ii,itest)-(jvalo-1)*nxo
      endif
c      add up the weights of contributing gridpoints which are not land
      if(ilando(ivalo,jvalo).ne.0)then
      nsea=nsea+1
      sumw=sumw+wt(ii,itest)
      else
      nland=nland+1
      endif
      endif
      end do
c      ------------------
      if(sumtot.eq.0.)then
      ofrac(ival,jval)=0.
      else
      ofrac(ival,jval)=sumw/sumtot
      endif
      nsum=0
      sumnew=0.
      do ii=1,48
      if(wt(ii,itest).gt.0.)then
            if(mod(iadress(ii,itest),nxo).eq.0)then
            jvalo=iadress(ii,itest)/nxo
            ivalo=nxo
            else
            jvalo=iadress(ii,itest)/nxo+1
            ivalo=iadress(ii,itest)-(jvalo-1)*nxo
            endif
c     for the sea points change the weights so that land
c     points do not contribute

        if(nsea.ne.0)then

            if(ilando(ivalo,jvalo).ne.0)then
            nsum=nsum+1
            sumnew=sumnew+wt(ii,itest)/sumw
            iadressnew(nsum,itest)=iadress(ii,itest)
            wtnew(nsum,itest)=wt(ii,itest)/sumw
            else
            endif

        else

            nsum=nsum+1
            sumnew=sumnew+wt(ii,itest)
            iadressnew(nsum,itest)=iadress(ii,itest)
            wtnew(nsum,itest)=wt(ii,itest)

        endif

      endif

      end do

      end do !itest
      open(11,file="OUTPUT/ofrac.igcm3")
      do j=nya,1,-1
      write(11,"(e16.8)")(ofrac(i,j),i=1,nxa)
      end do
      close(11)
      knumb=2
      open(unit=90, file='OUTPUT/at31topa',form='unformatted')
      write(clweight,"(A7,I1)")"WEIGHTS",knumb
      write(cladress,"(A7,I1)")"ADRESSE",knumb
      write(90) cladress
      write(90) iadressnew
      write(90) clweight
      write(90)wtnew
      open(91,file="atmosint.dat")
      open(92,file="atmoschk.dat")
      do jt=1,nya
      do it=1,nxa
      ii=(jt-1)*nxa+it
      atmosint(it,jt)=0.
      do k=1,20
      ival=mod(iadressnew(k,ii),180)
      jval=iadressnew(k,ii)/180+1
      if(ival.eq.0)then
      ival=180
      jval=jval-1
      endif
      if(wt(k,ii).gt.0.)then
      atmosint(it,jt)=atmosint(it,jt)+wtnew(k,ii)*atopa(it,jt)
     1   *ofrac(it,jt)
c    1   *rmasko(ival,jval)
      if(it.eq.4 .and.jt.eq.32)then
      write(6,*) " Atmos value = ",atmosint(it,jt),k,
     1   wt(k,ii),atopa(ival,jval),ival,jval
      endif
      endif
      end do
      write(91,*)atmosint(it,jt)
      write(92,*)atopa(it,jt)*ofrac(it,jt)
      end do !it
      end do !jt
      write(6,*) " atmosint ",atmosint(4,32)
      close(91)
      close(92)
c-------------------------------------------------
      write(6,*) " second bit "
      do is=1,nxo
      do js=1,nyo
c     do is=24,24
c     do js=20,20

      iada=(js-1)*nxo+is
      kk=0
c     write(6,*) " ----------------------- "
c     write(6,*)rlats(js),rlat(js),rlatn(js)
c     write(6,*)rlonw(is),rlon(is),rlone(is)
c     write(6,*)atopa(is,js)
      do i=1,nxa
      do j=1,nya
      if
     $ ( rlonow(is) .gt. rlonw(i) 
     $            .and. 
     $   rlonoe(is) .lt. rlone(i) )
     $ then
      rlow=rlonow(is)
      rloe=rlonoe(is)
c     write(6,*) " Ai = ",i
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c----
      if
     $ ( rlatos(js) .gt. rlats(j) 
     $            .and. 
     $   rlaton(js) .lt. rlatn(j) )
     $ then
      rlan=rlaton(js)
      rlas=rlatos(js)
      dlon=rloe-rlow
      h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
      weight=pi*(2.d0*re*h)*dlon/360.d0
      if(weight.lt.0.d0)weight=-weight
      iad=(j-1)*nxa+i
      kk=kk+1
      wt1(kk,iada)=weight/atopao(is,js)
      iadress1(kk,iada)=iad
c     write(6,*)" AA ",i,j,iad,weight,kk
c     write(6,*) " AAj = ",j
c     write(6,*) " AAi = ",i
c     write(6,*) " rlan = ",rlan
c     write(6,*) " rlas = ",rlas
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c     write(6,*) " ------- "
      else if
     $ ( rlatos(js) .gt. rlats(j) 
     $            .and.
     $   rlatos(js) .lt. rlatn(j) 
     $            .and.
     $   rlaton(js) .ge. rlatn(j) )
     $ then
      rlan=rlatn(j)
      rlas=rlatos(js)
      dlon=rloe-rlow
      h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
      weight=pi*(2.d0*re*h)*dlon/360.d0
      if(weight.lt.0.d0)weight=-weight
      iad=(j-1)*nxa+i
      kk=kk+1
      wt1(kk,iada)=weight/atopao(is,js)
      iadress1(kk,iada)=iad
c     write(6,*)" AB ",i,j,iad,weight,kk
c     write(6,*) " ABj = ",j
c     write(6,*) " ABi = ",i
c     write(6,*) " rlan = ",rlan
c     write(6,*) " rlas = ",rlas
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c     write(6,*) " ------- "
      else if
     $ ( rlatos(js) .le. rlats(j) 
     $            .and.
     $   rlaton(js) .gt. rlats(j)  
     $            .and.
     $   rlaton(js) .lt. rlatn(j) )
     $ then
      rlan=rlaton(js)
      rlas=rlats(j)
      dlon=rloe-rlow
      h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
      weight=pi*(2.d0*re*h)*dlon/360.d0
      if(weight.lt.0.d0)weight=-weight
      iad=(j-1)*nxa+i
      kk=kk+1
      wt1(kk,iada)=weight/atopao(is,js)
      iadress1(kk,iada)=iad
c     write(6,*)" AC ",i,j,iad,weight,kk
c     write(6,*) " ACj = ",j
c     write(6,*) " ACi = ",i
c     write(6,*) " rlan = ",rlan
c     write(6,*) " rlas = ",rlas
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c     write(6,*) " ------- "
      endif
c----
      else if
     $ ( rlonow(is) .gt. rlonw(i) 
     $            .and.
     $   rlonow(is) .lt. rlone(i) 
     $            .and.
     $   rlonoe(is) .ge. rlone(i) )
     $ then
      rlow=rlonow(is)
      rloe=rlone(i)
c     write(6,*) " Bi = ",i
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c----
      if
     $ ( rlatos(js) .gt. rlats(j) 
     $            .and. 
     $   rlaton(js) .lt. rlatn(j) )
     $ then
      rlan=rlaton(js)
      rlas=rlatos(js)
      dlon=rloe-rlow
      h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
      weight=pi*(2.d0*re*h)*dlon/360.d0
      if(weight.lt.0.d0)weight=-weight
      iad=(j-1)*nxa+i
      kk=kk+1
      wt1(kk,iada)=weight/atopao(is,js)
      iadress1(kk,iada)=iad
c     write(6,*)" BA ",i,j,iad,weight,kk
c     write(6,*) " BAj = ",j
c     write(6,*) " BAi = ",i
c     write(6,*) " rlan = ",rlan
c     write(6,*) " rlas = ",rlas
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c     write(6,*) " ------- "
      else if
     $ ( rlatos(js) .gt. rlats(j) 
     $            .and.
     $   rlatos(js) .lt. rlatn(j) 
     $            .and.
     $   rlaton(js) .ge. rlatn(j) )
     $ then
      rlan=rlatn(j)
      rlas=rlatos(js)
      dlon=rloe-rlow
      h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
      weight=pi*(2.d0*re*h)*dlon/360.d0
      if(weight.lt.0.d0)weight=-weight
      iad=(j-1)*nxa+i
      kk=kk+1
      wt1(kk,iada)=weight/atopao(is,js)
      iadress1(kk,iada)=iad
c     write(6,*)" BB ",i,j,iad,weight,kk
c     write(6,*) " BBj = ",j
c     write(6,*) " BBi = ",i
c     write(6,*) " rlan = ",rlan
c     write(6,*) " rlas = ",rlas
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c     write(6,*) " ------- "
      else if
     $ ( rlatos(js) .le. rlats(j) 
     $            .and.
     $   rlaton(js) .gt. rlats(j)  
     $            .and.
     $   rlaton(js) .lt. rlatn(j) )
     $ then
      rlan=rlaton(js)
      rlas=rlats(j)
      dlon=rloe-rlow
      h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
      weight=pi*(2.d0*re*h)*dlon/360.d0
      if(weight.lt.0.d0)weight=-weight
      iad=(j-1)*nxa+i
      kk=kk+1
      wt1(kk,iada)=weight/atopao(is,js)
      iadress1(kk,iada)=iad
c     write(6,*)" BC ",i,j,iad,weight,kk
c     write(6,*) " BCj = ",j
c     write(6,*) " BCi = ",i
c     write(6,*) " rlan = ",rlan
c     write(6,*) " rlas = ",rlas
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c     write(6,*) " ------- "
      endif
c----
      else if
     $ ( rlonow(is) .le. rlonw(i) 
     $            .and.
     $   rlonoe(is) .gt. rlonw(i) 
     $            .and.
     $   rlonoe(is) .lt. rlone(i) )
     $ then
      rlow=rlonw(i)
      rloe= rlonoe(is)
c     write(6,*) " Ci = ",i
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c----
      if
     $ ( rlatos(js) .gt. rlats(j) 
     $            .and. 
     $   rlaton(js) .lt. rlatn(j) )
     $ then
      rlan=rlaton(js)
      rlas=rlatos(js)
      dlon=rloe-rlow
      h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
      weight=pi*(2.d0*re*h)*dlon/360.d0
      if(weight.lt.0.d0)weight=-weight
      iad=(j-1)*nxa+i
      kk=kk+1
      wt1(kk,iada)=weight/atopao(is,js)
      iadress1(kk,iada)=iad
c     write(6,*)" CA ",i,j,iad,weight,kk
c     write(6,*) " CAj = ",j
c     write(6,*) " CAi = ",i
c     write(6,*) " rlan = ",rlan
c     write(6,*) " rlas = ",rlas
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c     write(6,*) " ------- "
      else if
     $ ( rlatos(js) .gt. rlats(j) 
     $            .and.
     $   rlatos(js) .lt. rlatn(j) 
     $            .and.
     $   rlaton(js) .ge. rlatn(j) )
     $ then
      rlan=rlatn(j)
      rlas=rlatos(js)
      dlon=rloe-rlow
      h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
      weight=pi*(2.d0*re*h)*dlon/360.d0
      if(weight.lt.0.d0)weight=-weight
      iad=(j-1)*nxa+i
      kk=kk+1
      wt1(kk,iada)=weight/atopao(is,js)
      iadress1(kk,iada)=iad
c     write(6,*)" CB ",i,j,iad,weight,kk
c     write(6,*) " CBj = ",j
c     write(6,*) " CBi = ",i
c     write(6,*) " rlan = ",rlan
c     write(6,*) " rlas = ",rlas
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c     write(6,*) " ------- "
      else if
     $ ( rlatos(js) .le. rlats(j) 
     $            .and.
     $   rlaton(js) .gt. rlats(j)  
     $            .and.
     $   rlaton(js) .lt. rlatn(j) )
     $ then
      rlan=rlaton(js)
      rlas=rlats(j)
      dlon=rloe-rlow
      h=re*(dsin(pi*rlas/180.d0)-dsin(pi*rlan/180.d0))
      weight=pi*(2.d0*re*h)*dlon/360.d0
      if(weight.lt.0.d0)weight=-weight
      iad=(j-1)*nxa+i
      kk=kk+1
      wt1(kk,iada)=weight/atopao(is,js)
      iadress1(kk,iada)=iad
c     write(6,*)" CC ",i,j,iad,weight,kk
c     write(6,*) " CCj = ",j
c     write(6,*) " CCi = ",i
c     write(6,*) " rlan = ",rlan
c     write(6,*) " rlas = ",rlas
c     write(6,*) " rlow = ",rlow
c     write(6,*) " rloe = ",rloe
c     write(6,*) " ------- "
c     write(6,*) " rlatos = ",rlatos(j)
c     write(6,*) " rlaton = ",rlaton(j)
c     write(6,*) " rlonow = ",rlonow(i)
c     write(6,*) " rlonoe = ",rlonoe(i)
      endif
c----
      endif
      end do
      end do
c     write(6,*) " iada = ",iada
c     summ=0.d0
c     do k=1,48
c     summ=summ+wt(k,iada)
c     write(6,*)k, wt(k,iada),iadress(k,iada)
c     end do
c     write(6,*) " summ = ",summ

      end do
      end do
      knumb=1
      open(unit=90, file='OUTPUT/at31topa',form='unformatted')
      write(clweight,"(A7,I1)")"WEIGHTS",knumb
      write(cladress,"(A7,I1)")"ADRESSE",knumb
      write(90) cladress
      write(90) iadress1
      write(90) clweight
      write(90)wt1
      return
      end

      SUBROUTINE GWTLT(SIT,WEIGHT,J,JG)
      PARAMETER(PI=3.14159265359)
      DOUBLE PRECISION ACC,SA,SB,SC,D1,D2,D4,BN,GG,X,AMM,AMN,
     1 EM,ANN,RE,A,DD,DLT,DTH
      ACC=1.0D-16
      SA=DSQRT(.5D0)
      SB=DSQRT(1.5D0)
      SC=DSQRT(1.D0/3.D0)
      D1=1.D0
      D2=2.D0
      D4=4.D0
      NNN=JG+JG
      BN=NNN
      GG=D2*BN+D1
      HH=8.*BN*BN
      K=0
      AJ=J
      TH=PI*(2.*AJ-.5)/GG
      DTH=TH+(COS(TH)/SIN(TH))/HH
      X=DCOS(DTH)
54    CONTINUE
      AMM=SA
      AMN=X*SB
      EM=SC
      ANN=D1
      DO 51 N=2,NNN
      ANN=ANN+D1
      RE=DSQRT(D4*ANN*ANN-D1)/ANN
      A=RE*(X*AMN-EM*AMM)
      AMM=AMN
      AMN=A
      EM=D1/RE
51    CONTINUE
      DD=GG*EM*AMM-X*ANN*A
      K=K+1
      DLT=(D1-X*X)*A/DD
      IF (DABS(DLT).LT.ACC) GOTO 52
      IF (K.GT.50) GOTO 53
      X=X-DLT
      GOTO 54
53    CONTINUE
      WRITE (2,105)
105   FORMAT(15H NO CONVERGENCE)
52    CONTINUE
      WEIGHT=GG*(D1-X*X)/(DD*DD)
      SIT=X
      RETURN
      END
