      program mosaicweights
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
      double precision pi, re,h,h1
      double precision wtnew(48,nxa*nya)
      double precision wt(48,nxa*nya)
      double precision wt1(48,nxo*nyo)
      double precision gwt(nya)
      double precision ofrac(nxa,nya)
      integer iadressnew(48,nxa*nya)
      integer iadress(48,nxa*nya)
      integer iadress1(48,nxo*nyo)
      integer iland(nxa,nya)
      integer ilando(nxo,nyo)
      integer ioland(nxa,nya)
      integer iosea(nxa,nya)
      character*8 cladress,clweight
      character*8 cladress1,clweight1
      do i=1,nxa
      do j=1,nya
      ioland(i,j)=0
      iosea(i,j)=1
      end do
      end do

      knumb=1
      open(unit=90, file='INPUT/at31topa',form='unformatted')
      read(90) cladress
      read(90) iadress
      read(90) clweight
      read(90)wt
      write(6,"(a8)")cladress
      write(6,"(a8)")clweight
      read(90) cladress1
      read(90) iadress1
      read(90) clweight1
      read(90)wt1
      write(6,"(a8)")cladress1
      write(6,"(a8)")clweight1
c      choose a igcm gridbox
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
c      read in the igcm land mask
      open(1,file="OUTPUT/igcm3mask.i1")
      do j=nya,1,-1
      read(1,"(128i1)")(iland(i,j),i=1,nxa)
      end do
      close(1)
      if(mod(itest,100).eq.0)write(6,*)itest
c      read in the ocean land mask
      open(1,file="INPUT/ocean.kmtc")
      read(1,*)
      open(2,file="checkkmt.dat")
      do j=nyo,1,-1
      read(1,"(1x,100i3)")(ilando(i,j),i=1,nxo)
      write(2,"(i3)")(ilando(i,j),i=1,nxo)
      end do
      close(1)
      close(2)
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

      open(unit=91, file='OUTPUT/at31topa.new',form='unformatted')
      write(91) cladress
      write(91) iadressnew
      write(91) clweight
      write(91)wtnew
      write(91) cladress1
      write(91) iadress1
      write(91) clweight1
      write(91)wt1
      do i=1,20
c     write(6,*)i,wtnew(i,1151),wt(i,1151)
      io=mod(iadressnew(i,1151),nxo)
      jo=1+iadressnew(i,1151)/nxo
      write(6,*)i,wtnew(i,1151),iadressnew(i,1151),io,jo
     1   ,ilando(io,jo)
      end do
      stop
      end

