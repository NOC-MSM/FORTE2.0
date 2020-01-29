      program mosaicweights
c      calculates oasis mozaic interpolation
c      weights between igcm3 and moma grids

      parameter(nxo=90,nyo=45)
      parameter(nxo2=180,nyo2=90)
      integer ilando(nxo,nyo)
      integer ilando2(nxo2,nyo2)
c      read in the ocean land mask
      open(1,file="INPUT/ocean.kmtc")
      read(1,*)
      do j=nyo,1,-1
      read(1,"(1x,90i3)")(ilando(i,j),i=1,nxo)
      end do
      do j=1,nyo
      do i=1,nxo
      ilando2(2*i-1,2*j-1)=ilando(i,j)
      end do
      do i=1,nxo
      ilando2(2*i,2*j-1)=ilando2(2*i-1,2*j-1)
      end do
      end do
c---
      do i=1,nxo2
      do j=1,nyo
      ilando2(i,2*j)=ilando2(i,2*j-1)
      end do
      end do
c---
      close(1)
c      write out ocean model mask
      do j=45,1,-1
      write(16,"(90i1)")(ilando(i,j),i=1,90)
      end do
c---
      do j=90,1,-1
      write(17,"(90i1)")(ilando2(i,j),i=1,90)
      write(18,"(90i1)")(ilando2(i,j),i=91,180)
      end do
c---
      open(1,file="ocean.kmtc.2x2")
      write(1,"(a9,3i10)")" kmt     ",nxo2,nyo2,15
      do j=nyo2,1,-1
      write(1,"(1x,90i3)")(ilando2(i,j),i=1,nxo2)
      end do
      close(1)

      stop
      end

