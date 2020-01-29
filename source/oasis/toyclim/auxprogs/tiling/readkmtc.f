      program mosaicweights
c      calculates oasis mozaic interpolation
c      weights between igcm3 and moma grids

      parameter(nxo=90,nyo=45)
      parameter(nxo2=180,nyo2=45)
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
      ilando2(2*i-1,j)=ilando(i,j)
      end do
      do i=1,nxo
      ilando2(2*i,j)=ilando2(2*i-1,j)
      end do
      end do
      close(1)
c      write out ocean model mask
      do j=45,1,-1
      write(16,"(90i1)")(ilando(i,j),i=1,90)
      write(17,"(90i1)")(ilando2(i,j),i=1,90)
      write(18,"(90i1)")(ilando2(i,j),i=91,180)
c     write(16,"(i16)")(ilando(i,j),i=1,90)
      end do
      open(1,file="ocean.kmtc.2x4")
      write(1,"(a9,3i10)")" kmt     ",nxo2,nyo2,15
      do j=nyo2,1,-1
      write(1,"(1x,90i3)")(ilando2(i,j),i=1,nxo2)
      end do
      close(1)

      stop
      end

