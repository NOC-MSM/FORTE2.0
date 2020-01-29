      program mosaicweights
c      calculates oasis mozaic interpolation
c      weights between igcm3 and moma grids

      parameter(jg=32,ig=128)
      parameter(nxo=180,nyo=88)
      parameter(nya=2*jg,nxa=ig)
      double precision wt(48,nxa*nya)
      double precision wt1(48,nxo*nyo)
      double precision ofrac(nxa,nya)
      integer iadress(48,nxa*nya)
      integer iadress1(48,nxo*nyo)
      integer ilando(nxo,nyo)
      character*8 cladress,clweight

      open(1,file="INPUT/ocean.kmtc")
      read(1,*)
      do j=nyo,1,-1
      read(1,"(1x,100i3)")(ilando(i,j),i=1,nxo)
      end do
      close(1)
c      read in the ocean land mask
      do j=nyo,1,-1
      write(6,"(90i1)")(10*ilando(i,j),i=1,60)
      end do
      write(6,*)
      do j=nyo,1,-1
      write(6,"(90i1)")(10*ilando(i,j),i=61,120)
      end do
      write(6,*)
      do j=nyo,1,-1
      write(6,"(90i1)")(10*ilando(i,j),i=121,180)
      end do
      open(unit=90, file='INPUT/at31topa',form='unformatted')
      read(90) cladress
      read(90) iadress
      read(90) clweight
      read(90)wt
      write(6,"(a8)")cladress
      write(6,"(a8)")clweight
      do itest = 1,8192
c      convert the address to a i,j value
c      for the igcm
      if(mod(itest,ig).eq.0)then
      jval=itest/ig
      ival=ig
      else
      jval=itest/jg+1
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
      if(itest.eq.769)write(6,"(2i10,f10.2,5i10)")
     1   itest,ii,wt(ii,itest),iadress(ii,itest),
     1 ivalo,jvalo   ,ilando(ivalo,jvalo)
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
c      ------------------
      if(sumtot.eq.0.)then
      ofrac(ival,jval)=0.
      else
      ofrac(ival,jval)=sumw/sumtot
c     ofrac(ival,jval)=1.
      endif

      if(itest.eq.769)write(6,*)itest,ofrac(ival,jval)

      end do !itest

      open(11,file="OUTPUT/ofrac.igcm3")
      do j=nya,1,-1
      write(11,"(e16.8)")(ofrac(i,j),i=1,nxa)
      end do
      close(11)
      stop
      end

