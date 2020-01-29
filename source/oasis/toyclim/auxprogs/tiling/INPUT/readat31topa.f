      
      parameter(jg=16,ig=64)
      parameter(nxo=180,nyo=88)
      parameter(nya=2*jg,nxa=ig)
      double precision wt(48,nxa*nya)
      double precision wt1(48,nxo*nyo)
      double precision gwt(nya)
      integer iadress(48,nxa*nya)
      integer iadress1(48,nxo*nyo)
      character*8 cladress,clweight
      open(unit=90, file='at31topa',form='unformatted')
      read(90) cladress
      write(6,"(a8)")cladress
      read(90) iadress
      read(90) clweight
      write(6,"(a8)")clweight
      read(90)wt
      write(6,*) " got to here "
      read(90) cladress
      write(6,"(a8)")cladress
      read(90) iadress1
      read(90) clweight
      write(6,"(a8)")clweight
      read(90)wt1
      close(90)
      write(6,*) " enter ocean i,j indices (e.g. 1, 6) "
      read(5,*)ii,jj
      iindex=(jj-1)*nxo+ii
      write(6,*) " ocean index for weights = ",iindex
      do i=1,10
      ia=mod(iadress1(i,iindex),ig)
      ja=1+iadress1(i,iindex)/ig
      write(6,*)i,wt1(i,iindex),iadress1(i,iindex),ia,ja
     1   ,ig*(nya-ja)+ia
      end do
      stop
      end
