c     subroutine mosaicweights
c      calculates oasis mozaic interpolation
c      weights between igcm3 and moma grids

      parameter(jg=32,ig=128)
      parameter(nxo=180,nyo=88)
      parameter(yoffs=-87.,xoffs=1.)
      parameter(dyo=2.,dxo=2.)
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
      double precision pi, re,h,h1,sumo
      double precision wtnew(48,nxa*nya)
      double precision wt(48,nxa*nya)
      double precision wt1(48,nxo*nyo)
      double precision fin(nxa,nya)
      double precision fou(nxo,nyo)
      double precision ofrac(nxa,nya)
      double precision gwt(nya)
      real*8 atmosint(nxa,nya)
      integer iadressnew(48,nxa*nya)
      integer iadress(48,nxa*nya)
      integer iadress1(48,nxo*nyo)
      integer iland(nxa,nya)
      integer ilando(nxo,nyo)
      character*8 cladress,clweight

      do j=1,nya
      do i=1,nxa
      read(83,*)fin(i,j)
      end do
      end do


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
      end do

      do j=1,jg
        wn2(j)=-wn2(2*jg+1-j)
      end do

      do j=1,2*jg
        if(j.le.jg)then
          wn3(j)=wn2(j)
        else
          wn3(j+1)=wn2(j)
        endif
      end do

      wn3(jg+1)=0.


      w(1)=2.d0*rl2(1)
      do j=2,jg
        w(j)=2.d0*(rl2(j)-rl2(j-1)-w(j-1)/2.)
      end do

      do j=1,jg
        w2(j)=w(jg+1-j)
        w2(j+jg)=w(j)
      end do


      do j=1,nya
        rlatn(j)=rlat(j)+w2(j)/2.d0
        rlats(j)=rlat(j)-w2(j)/2.d0
      end do

      do j=1,nya
        rlatn(j)=wn3(j+1)
        rlats(j)=wn3(j)
      write(6,*)j, rlatn(j),rlats(j)
      end do

      dx=360.d0/real(nxa)
      do i=1,nxa
        rlon(i)=dx/2.d0+real(i-1)*dx
      end do
      do i=1,nxa
        rlonw(i)=real(i-1)*dx
        rlone(i)=real(i)*dx
      end do

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
      end do
      end do

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
          rlow=rlonow(i)
          rloe=rlone(is)
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
     $ ( rlonow(i) .le. rlonw(is)
     $            .and.
     $   rlonoe(i) .gt. rlonw(is)
     $            .and.
     $   rlonoe(i) .lt. rlone(is) )
     $THEN
      rlow=rlonw(is)
      rloe= rlonoe(i)
c----
      if
     $ ( rlatos(j) .gt. rlats(js)
     $            .and.
     $   rlaton(j) .lt. rlatn(js) )
     $ then
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
     $ then
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
     $ then
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
      END IF
      end do
      end do

      END DO
      END DO
      sumo=0.
      do j=2,nyo-1
      do i=1,nxo
!     sumo=sumo+atopao(i,j)
      sumo=sumo+atopao(i,j)*real(ilando(i,j))
      end do
      end do
      write(6,*) " SUMO = ",sumo
      suma=0.
      do j=2,nya-1
      do i=1,nxa
      if(j.eq.2 .or. j.eq.nya-1)then
!     suma=suma+atopa(i,j)*0.8181818181d0
!     suma=suma+atopa(i,j)*0.842598320000000d0
      suma=suma+atopa(i,j)*0.842617848118443d0
      else
      suma=suma+atopa(i,j)
      endif
      end do
      end do
      write(6,*) " SUMA = ",suma

      open(11,file="OUTPUT/ofrac.igcm3")
      do j=nya,1,-1
      read(11,*)(ofrac(i,j),i=1,nxa)
      end do
      close(11)


c-----------------
c      read in the igcm land mask
      open(1,file="working/igcm3.1mask.i1")
      do j=nya,1,-1
      read(1,"(128i1)")(iland(i,j),i=1,nxa)
      end do
      close(1)
      if(mod(itest,100).eq.0)write(6,*)itest
c      read in the ocean land mask
      open(1,file="ocean.kmtc.arctic_ridge_dp_ds")
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

      if(ival.eq.100 .and. jval.eq.63)then
      write(6,*) " eewtes ", ival,jval,sumw,wt(ii,itest)
      write(6,*) ii,iadress(ii,itest),ivalo,jvalo,ilando(ivalo,jvalo)
      endif
      if(ival.eq.101 .and. jval.eq.63)then
      write(6,*) " eewtes ", ival,jval,sumw,wt(ii,itest)
      write(6,*) ii,iadress(ii,itest),ivalo,jvalo,ilando(ivalo,jvalo)
      endif
      if(ival.eq.102 .and. jval.eq.63)then
      write(6,*) " eewtes ", ival,jval,sumw,wt(ii,itest)
      write(6,*) ii,iadress(ii,itest),ivalo,jvalo,ilando(ivalo,jvalo)
      endif
      if(ival.eq.103 .and. jval.eq.63)then
      write(6,*) " eewtes ", ival,jval,sumw,wt(ii,itest)
      write(6,*) ii,iadress(ii,itest),ivalo,jvalo,ilando(ivalo,jvalo)
      endif

      endif
      end do
      if(ival.eq.100 .and. jval.eq.63)then
      write(6,*) " newtes ", ival,jval,nsea,nland
      write(6,*) sumw,sumtot
      endif
      if(ival.eq.101 .and. jval.eq.63)then
      write(6,*) " newtes ", ival,jval,nsea,nland
      write(6,*) sumw,sumtot
      endif
      if(ival.eq.102 .and. jval.eq.63)then
      write(6,*) " newtes ", ival,jval,nsea,nland
      write(6,*) sumw,sumtot
      endif
      if(ival.eq.103 .and. jval.eq.63)then
      write(6,*) " newtes ", ival,jval,nsea,nland
      write(6,*) sumw,sumtot
      endif

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
c     if(ival.eq.100 .and. jval.eq.63)then
c     write(6,*) " ofractest ", ival,jval,ofrac(ival,jval)
c     write(6,*) itest
c     do ii=1,10
c     write(6,*)ii,wt(ii,itest),wtnew(ii,itest)
c     end do
c     endif
c     if(ival.eq.101 .and. jval.eq.63)then
c     write(6,*) " ofractest ", ival,jval,ofrac(ival,jval)
c     write(6,*) itest
c     do ii=1,10
c     write(6,*)ii,wt(ii,itest),wtnew(ii,itest)
c     end do
c     endif
c     if(ival.eq.102 .and. jval.eq.63)then
c     write(6,*) " ofractest ", ival,jval,ofrac(ival,jval)
c     write(6,*) itest
c     do ii=1,10
c     write(6,*)ii,wt(ii,itest),wtnew(ii,itest)
c     end do
c     endif
c     if(ival.eq.103 .and. jval.eq.63)then
c     write(6,*) " ofractest ", ival,jval,ofrac(ival,jval)
c     write(6,*) itest
c     do ii=1,10
c     write(6,*)ii,wt(ii,itest),wtnew(ii,itest)
c     end do
c     endif

      end do !itest
      open(unit=90, file='OUTPUT/at31topa',form='unformatted')
      read(90) cladress
      read(90) iadressnew
      read(90) clweight
      read(90)wtnew
c-------------------------------------------------
      open(unit=90, file='OUTPUT/at31topa',form='unformatted')
      read(90) cladress
      read(90) iadress1
      read(90) clweight
      read(90)wt1

c     do j=1,nya
c     do i=1,nxa
c     if(iland(i,j).eq.0. .and. ofrac(i,j).lt.1.
c    1  .and. ofrac(i,j).gt.0.)fin(i,j)=fin(i,j)/ofrac(i,j)
c     end do
c     end do

      itest=1

      do ii=1,48
      if(wtnew(ii,itest).gt.0.)then
      if(mod(iadressnew(ii,itest),nxo).eq.0)then
      jvalo=iadressnew(ii,itest)/nxo
      ivalo=nxo
      else
      jvalo=iadressnew(ii,itest)/nxo+1
      ivalo=iadressnew(ii,itest)-(jvalo-1)*nxo
      endif
      write(6,*)ii,ivalo,jvalo
      endif
      end do

      do i=1,10
      write(6,*)i,wtnew(i,1),iadressnew(i,1)
      end do

      do j=1,nyo
      do i=1,nxo
c     do j=50,50
c     do i=100,100
      itest=(j-1)*nxo+i
      write(6,*) " itest = ",itest

      if(wt1(1,itest).gt.0.)then
      if(mod(iadress1(1,itest),nxa).eq.0)then
      jvalo=iadress1(1,itest)/nxa
      ivalo=nxa
      else
      jvalo=iadress1(1,itest)/nxa+1
      ivalo=iadress1(1,itest)-(jvalo-1)*nxa
      endif
      write(6,*)1,ivalo,jvalo,wt1(1,itest)
      endif

      if(ivalo.ne.0 .and. jvalo.ne.0)then
      fou(i,j)=wt1(1,itest)*fin(ivalo,jvalo)
      endif
      do ii=2,48
      if(wt1(ii,itest).gt.0.)then
      if(mod(iadress1(ii,itest),nxa).eq.0)then
      jvalo=iadress1(ii,itest)/nxa
      ivalo=nxa
      else
      jvalo=iadress1(ii,itest)/nxa+1
      ivalo=iadress1(ii,itest)-(jvalo-1)*nxa
      endif
      write(6,*)ii,ivalo,jvalo,wt1(ii,itest)
      endif

      if(ivalo.ne.0 .and. jvalo.ne.0)then
      fou(i,j)=fou(i,j)+wt1(ii,itest)*fin(ivalo,jvalo)
c     write(6,*)ii,ivalo,jvalo,wt1(ii,itest)
      endif
      end do

      end do
      end do

      open(11,file="weights.test")
      do j=1,nyo
      do i=1,nxo
      write(11,*)fou(i,j)
      end do
      end do
      close(11)


      write(6,*) " atmos "
      write(6,*) rlatn(1),rlats(1)
      write(6,*) " ocean "
      write(6,*) rlaton(1),rlatos(1)
      write(6,*) rlaton(2),rlatos(2)

      write(6,*) " atmos "
      write(6,*) rlonw(1),rlone(1)
      write(6,*) " ocean "
      write(6,*) rlonow(1),rlonoe(1)
      write(6,*) rlonow(2),rlonoe(2)

c     do i=1,128
c     write(6,*)  " ofrac ", ofrac(i,2)
c     end do

      sumo=0.
      do j=2,nyo-1
      do i=1,nxo
      sumo=sumo+atopao(i,j)
      end do
      end do
      write(6,*) " SUMO = ",sumo

      sumo=0.
      do j=2,nyo-1
      do i=1,nxo
!     sumo=sumo+atopao(i,j)
      if(ilando(i,j).ne.0)then
      sumo=sumo+atopao(i,j)
      endif
      end do
      end do
      write(6,*) " SUMO1 = ",sumo

      sumo=0.
      do j=2,nyo-1
      do i=1,nxo
!     sumo=sumo+atopao(i,j)
c     if(ilando(i,j).ne.0)then
      sumo=sumo+atopao(i,j)*fin(i,j)
c     endif
      end do
      end do
      write(6,*) " SUMFIN = ",sumo

      suma=0.
      do j=2,nya-1
      do i=1,nxa
      if(j.eq.2 .or. j.eq.nya-1)then
!     suma=suma+atopa(i,j)*0.8181818181d0
!     suma=suma+atopa(i,j)*0.842598320000000d0
      suma=suma+atopa(i,j)*0.842617848118443d0
c     suma=suma+atopa(i,j)*ofrac(i,j)
      else
      suma=suma+atopa(i,j)
      endif
      end do
      end do
      write(6,*) " SUMA = ",suma

      suma=0.
      do j=2,nya-1
      do i=1,nxa
c     if(j.eq.2 .or. j.eq.nya-1)then
!     suma=suma+atopa(i,j)*0.8181818181d0
!     suma=suma+atopa(i,j)*0.842598320000000d0
c     suma=suma+atopa(i,j)*0.842617848118443d0
c     else
      suma=suma+atopa(i,j)*ofrac(i,j)
c     endif
      end do
      end do
      write(6,*) " SUMA1 = ",suma

      suma=0.
      do j=2,nya-1
      do i=1,nxa
      if(j.eq.2 .or. j.eq.nya-1)then
!     suma=suma+atopa(i,j)*0.8181818181d0
!     suma=suma+atopa(i,j)*0.842598320000000d0
      suma=suma+atopa(i,j)*0.842617848118443d0*fou(i,j)
      else
c     suma=suma+atopa(i,j)*ofrac(i,j)*fou(i,j)
      suma=suma+atopa(i,j)*fou(i,j)
      endif
      end do
      end do
      write(6,*) " SUMfou = ",suma

      stop
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
