      parameter(mg=128,jg=32,mgpp=mg+2,igc=mgpp*2)
      parameter(nrunoff=18)

      integer listx(nrunoff),listy(nrunoff)

      integer map(mg,2*jg)
      integer flipmap(mg,2*jg)
  
      real gsg(igc,jg)

      character*2 vers

      data listx/88,88,89,90,91,92,93,94,95,96,97
     1   ,98,99,100,101,101,100,100/

      data listy/22,23,24,24,25,26,27,27,27,28,28
     1,29,30,30,30,31,32,33/

      write(6,*) " input version number (e.g. 01) "
      read(5,"(a2)")vers

      read(3,*)
      do j=1,2*jg
      read (3,'(128I1)')(flipmap(i,j),i=1,mg)
      end do

      do j=1,2*jg
      do i=1,mg
      map(i,j)=flipmap(i,2*jg+1-j)
      end do
      end do

      do j=1,jg
      do i=1,mg
      gsg(i,j)=map(i,2*jg+1-j)
      end do
      enddo

      do j=jg,1,-1
      do i=1,mg
      gsg(i+mgpp,j)=map(i,j)
      end do
      enddo
      open(59,file='t42.59.'//vers)                

      write(59,*)((gsg(i,j),i=1,mg),j=1,jg),(adum,i=1,mg), 
     $          ((gsg(i+mgpp,j),i=1,mg),j=jg,1,-1)

      close(59)

  
      stop
      end



     
