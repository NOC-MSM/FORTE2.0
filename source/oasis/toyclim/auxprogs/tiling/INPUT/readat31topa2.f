      
      parameter(jg=16,ig=64)
      parameter(nxo=90,nyo=45)
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
      write(6,*) " enter atmos i,j indices (e.g. 1, 6) "
      read(5,*)ii,jj
      iindex=(jj-1)*64+ii
      write(6,*) " atmos index for weights = ",iindex
      do i=1,20
      io=mod(iadress(i,iindex),nxo)
      jo=1+iadress(i,iindex)/nxo
      write(6,*)i,wt(i,iindex),iadress(i,iindex),io,jo
      end do
      stop
      end
