      programsalin_offset 
c       when killing the thc with a freshwater
c      perturbation in the N. Atlantic
c      you have to conserve salt by adding an
c      opposite perturbation to the rest of the ocean
c      this program calculates how big the latter 
c      should be

      parameter(nxo=90,nyo=45,nzo=15)
      integer ilando(nxo,nyo)
      real*8 arealat(nyo)
      real*8 dz(nzo)
      real*8 vol(nxo,nyo,nzo)
      real*8 totvol,volna,totarea

      data arealat/
     &6900563347.0477448d0, 20668071190.160427d0, 34334886267.981636d0,
     &47834425231.236359d0, 61100919683.06366d0, 74069736596.038345d0,  
     &86677693197.780365d0, 98863364791.074875d0, 110567384008.81786d0,  
     &121732730045.86378d0, 132305006458.65811d0, 142232706179.23831d0,  
     &151467462452.4949d0, 159964284474.13916d0, 167681776581.37903d0,
     &174582339928.4277d0, 180632355664.30005d0, 185802348720.47763d0,  
     &190067131410.4754d0, 193405926141.72244d0, 195802466641.90283d0,  
     &197245077206.599d0, 197726729582.15042d0, 197245077206.599d0,  
     &195802466641.90283d0, 93405926141.72244d0, 190067131410.4754d0,  
     &185802348720.47763d0, 180632355664.30005d0, 174582339928.4277d0,
     &167681776581.37903d0, 159964284474.13916d0, 151467462452.4949d0,  
     &142232706179.23831d0, 132305006458.65811d0, 121732730045.86378d0,  
     &110567384008.81786d0, 98863364791.074875d0, 86677693197.780365d0,  
     &74069736596.038345d0, 61100919683.06366d0, 47834425231.236359d0,  
     &34334886267.981636d0, 20668071190.160427d0, 6900563347.0477448d0/

      data dz /30.00d0,  46.15d0,  68.93d0, 99.93d0, 140.63d0,
     & 192.11d0, 254.76d0, 327.95d0, 409.81d0, 497.11d0,
     & 585.36d0, 669.09d0, 742.41d0, 799.65d0, 836.10d0/

      totvol=0.d0
      do j=1,nyo
      totvol=totvol+arealat(j)*90.d0
      end do
      write(6,*) " totarea = ",totvol, .71d0*4.d0*3.14d0*6370d3**2

c      read in the ocean land mask
      open(1,file="INPUT/ocean.kmtc")
      read(1,*)
      do j=nyo,1,-1
      read(1,"(1x,90i3)")(ilando(i,j),i=1,nxo)
      end do
      close(1)
c      write out ocean model mask
      do j=45,1,-1
      write(16,"(90i1)")(ilando(i,j),i=1,90)
      end do

      totarea=0.d0
      do i=1,nxo
      do j=1,nyo

      if(ilando(i,j).eq.0)then

      do k=1,nzo
      vol(i,j,k)=0.d0
      end do

      else
      totarea=totarea+arealat(j)

      do k=1,ilando(i,j)
      vol(i,j,k)=arealat(j)*dz(k)
      end do

      if(ilando(i,j).ne.15)then
      do k=ilando(i,j)+1,nzo
      vol(i,j,k)=0.d0
      end do
      endif
      

      endif

      end do
      end do
      write(6,*) " totarea = ",totarea

      totvol=0.d0
      do i=1,nxo
      do j=1,nyo
      do k=1,nzo
      totvol=totvol+vol(i,j,k)
      end do
      end do
      end do
      write(6,*) " totvol = ",totvol

      volna=0.d0
      do k=1,7
      do j=36,45
      do i=70,90
      volna=volna+vol(i,j,k)
      end do
      end do
      end do
      write(6,*) " volna = ",volna

      write(6,*) " ratio = ",volna/totvol

      stop
      end

