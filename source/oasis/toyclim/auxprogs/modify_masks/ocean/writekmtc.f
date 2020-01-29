      program mosaicweights
c      calculates oasis mozaic interpolation
c      weights between igcm3 and moma grids

      parameter(nxo2=180,nyo2=88)
      integer ilando2(nxo2,nyo2)
      integer ilando3(nxo2,nyo2)
      character*2 vers
      write(6,*) " enter land mask version (e.g. 01) "
      read(5,"(a2)")vers
c      read in ocean model mask again
      do j=88,1,-1
      read(17,"(90i1)")(ilando3(i,j),i=1,90)
      read(18,"(90i1)")(ilando3(i,j),i=91,180)
      end do
c      write out ocean model mask
      do j=1,88,1
      do i=1,180
      if(ilando3(i,j).eq.0)ilando2(i,j)=15
      if(ilando3(i,j).ne.0)ilando2(i,j)=0
      end do
      end do
c      read in the ocean land mask
      open(1,file="ocean.kmtc.test"//vers)
      write(1,*) "kmt            180        88        15"
      do j=nyo2,1,-1
      write(1,"(1x,100i3)")(ilando2(i,j),i=1,100)
      write(1,"(1x,80i3)")(ilando2(i,j),i=101,180)
      end do
      close(1)

c---
      stop
      end

