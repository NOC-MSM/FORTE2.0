      program mosaicweights
c      calculates oasis mozaic interpolation
c      weights between igcm3 and moma grids

      parameter(jg=16,ig=64)
      parameter(nxo=90,nyo=45)
      parameter(yoffs=-92.,xoffs=2.)
      parameter(dyo=4.,dxo=4.)
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
      open(unit=90, file='OUTPUT/at31topa.new',form='unformatted')
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
c     itest=1262
      write(6,*) " input itest e.g. 1262 "
      read(5,*)itest
!     do itest = 1,2048
c      convert the address to a i,j value
c      for the igcm
      if(mod(itest,64).eq.0)then
      jval=itest/64
      ival=64
      else
      jval=itest/64+1
      ival=itest-(jval-1)*64
      endif
c      read in the igcm land mask
      open(1,file="OUTPUT/igcm3mask.i1")
      do j=nya,1,-1
      read(1,"(64i1)")(iland(i,j),i=1,nxa)
      end do
      close(1)
c     write(6,*)ival,jval,iland(ival,jval)
      if(mod(itest,10).eq.0)write(6,*)itest
c      read in the ocean land mask
      open(1,file="INPUT/ocean.kmtc")
      read(1,*)
      do j=nyo,1,-1
      read(1,"(1x,90i3)")(ilando(i,j),i=1,nxo)
      end do
      close(1)
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
      if(mod(iadress(ii,itest),90).eq.0)then
      jvalo=iadress(ii,itest)/90
      ivalo=90
      else
      jvalo=iadress(ii,itest)/90+1
      ivalo=iadress(ii,itest)-(jvalo-1)*90
      endif
      write(6,"(5i5,e16.8)")ii,iadress(ii,itest),ivalo,jvalo,
     1   ilando(ivalo,jvalo),wt(ii,itest)
c      add up the weights of contributing gridpoints which are not land
      if(ilando(ivalo,jvalo).ne.0)then
      nsea=nsea+1
      sumw=sumw+wt(ii,itest)
      else
      nland=nland+1
      endif
      endif
      end do
      write(6,*)sumw,sumtot
      nsum=0
      write(6,*) " ------------------ "
      write(6,*) " nsea = ",nsea," nland = ",nland
      sumnew=0.
      do ii=1,48
      if(wt(ii,itest).gt.0.)then
            if(mod(iadress(ii,itest),90).eq.0)then
            jvalo=iadress(ii,itest)/90
            ivalo=90
            else
            jvalo=iadress(ii,itest)/90+1
            ivalo=iadress(ii,itest)-(jvalo-1)*90
            endif
c     for the sea points change the weights so that land
c     points do not contribute

        if(nsea.ne.0)then

            if(ilando(ivalo,jvalo).ne.0)then
            nsum=nsum+1
            sumnew=sumnew+wt(ii,itest)/sumw
            write(6,"(6i5,e16.8)")ii,iadress(ii,itest),ivalo,jvalo,
     1      ilando(ivalo,jvalo),nsum,wt(ii,itest)/sumw
            iadressnew(nsum,itest)=iadress(ii,itest)
            wtnew(nsum,itest)=wt(ii,itest)/sumw
            else
            write(6,"(6i5,e16.8)")ii,iadress(ii,itest),ivalo,jvalo,
     1      ilando(ivalo,jvalo),nsum,0.
            endif

        else

            nsum=nsum+1
            sumnew=sumnew+wt(ii,itest)
            write(6,"(6i5,e16.8)")ii,iadress(ii,itest),ivalo,jvalo,
     1      ilando(ivalo,jvalo),nsum,wt(ii,itest)
            iadressnew(nsum,itest)=iadress(ii,itest)
            wtnew(nsum,itest)=wt(ii,itest)

        endif

      endif

      end do
      write(6,*)" sumnew = ",sumnew
      
      write(6,* ) " NEW WEIGHTS "
c      go through the ocean adresses for this igcm
c      gridpoint, and work out the i,j values of
c      each contributing ocean gridpoint
      sumw=0.
      sumtot=0.
      nland=0
      nsea=0
      do ii=1,48
      if(wtnew(ii,itest).gt.0.)then
      sumtot=sumtot+wtnew(ii,itest)
      if(mod(iadressnew(ii,itest),90).eq.0)then
      jvalo=iadressnew(ii,itest)/90
      ivalo=90
      else
      jvalo=iadressnew(ii,itest)/90+1
      ivalo=iadressnew(ii,itest)-(jvalo-1)*90
      endif
      write(6,"(5i5,e16.8)")ii,iadressnew(ii,itest),ivalo,jvalo,
     1   ilando(ivalo,jvalo),wtnew(ii,itest)
c      add up the weights of contributing gridpoints which are not land
      if(ilando(ivalo,jvalo).ne.0)then
      nsea=nsea+1
      sumw=sumw+wtnew(ii,itest)
      else
      nland=nland+1
      endif
      endif
      end do
      write(6,*)sumw,sumtot
      write(6,*) " nsea = ",nsea," nland = ",nland
!     end do !itest

      stop
      end

