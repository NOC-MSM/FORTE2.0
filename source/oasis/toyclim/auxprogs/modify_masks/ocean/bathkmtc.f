      program mosaicweights
c      calculates oasis mozaic interpolation
c      weights between igcm3 and moma grids

      parameter(nxo2=180,nyo2=88)
      integer ilando2(nxo2,nyo2)
      integer ilando3(nxo2,nyo2)
      integer ilando4(nxo2,nyo2)
      character*2 vers
      write(6,*) " enter land mask version (e.g. 01) "
      read(5,"(a2)")vers

c      read in the old ocean land mask
c      open(1,file="ocean.kmtc.test"//vers)
      open(1,file="ocean.kmtc.test00")
      read(1,*)
      do j=nyo2,1,-1
      read(1,"(1x,100i3)")(ilando2(i,j),i=1,100)
      read(1,"(1x,80i3)")(ilando2(i,j),i=101,180)
      end do
      close(1)

c      read in the new ocean land mask
      open(1,file="ocean.kmtc")
      read(1,*)
      do j=nyo2,1,-1
      read(1,"(1x,100i3)")(ilando3(i,j),i=1,100)
      read(1,"(1x,80i3)")(ilando3(i,j),i=101,180)
      end do
      close(1)


c     copy bathymetry to new ocean model mask
      do j=1,88,1
      do i=1,180
c sets all new land points to zero depth
      if(ilando3(i,j).eq.0)then
      ilando4(i,j)=0
      endif
c replaces 1's with bathymetry from original map
      if(ilando3(i,j).ne.0)then

c ensures old land that has been removed is not replaced (set to 20)
      if(ilando2(i,j).ne.0)then
      ilando4(i,j)=ilando2(i,j)
      elseif(ilando2(i,j).eq.0)then
      ilando4(i,j)=20
      endif

      endif
      end do
      end do


c      write out original ocean model mask
      do j=1,88,1
      do i=1,180
      write(25,*)(ilando2(i,j))
      end do
      end do

c      write out new ocean model mask
      do j=1,88,1
      do i=1,180
      write(20,*)(ilando4(i,j))
      end do
      end do

c      write out ocean model mask again
      do j=88,1,-1
      write(21,"(90i3)")(ilando4(i,j),i=1,60)
      write(22,"(90i3)")(ilando4(i,j),i=61,120)
      write(23,"(90i3)")(ilando4(i,j),i=121,180)
      end do

c---
      write(6,*) ilando2(20,39)
      do j=88,74,-1
      write(6,"(90i3)")(ilando2(i,j),i=165,180)
      end do
      stop
      end
