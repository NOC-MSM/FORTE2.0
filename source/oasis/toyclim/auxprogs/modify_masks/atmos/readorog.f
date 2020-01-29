      parameter(mg=128,jg=32,mgpp=mg+2,igc=mgpp*2)
      parameter(nrunoff=18)

      integer listx(nrunoff),listy(nrunoff)

      integer map(mg,2*jg)
      integer flipmap(mg,2*jg)

      real gsg(igc,jg)
      real rap2(mg,2*jg)
      real fliprap2(mg,2*jg)

      character*2 vers

      data listx/88,88,89,90,91,92,93,94,95,96,97
     1   ,98,99,100,101,101,100,100/

      data listy/22,23,24,24,25,26,27,27,27,28,28
     1,29,30,30,30,31,32,33/

      write(6,*) " input version number (e.g. 01) "
      read(5,"(a2)")vers

      open(59,file='t42.59.'//vers)

      read(59,*)((gsg(i,j),i=1,mg),j=1,jg),(adum,i=1,mg),
     $          ((gsg(i+mgpp,j),i=1,mg),j=jg,1,-1)

      close(59)

c read new mask with topography
      do j=1,jg
      do i=1,mg
      rap2(i,2*jg+1-j)=gsg(i,j)
      end do
      enddo

      do j=jg,1,-1
      do i=1,mg
      rap2(i,j)=gsg(i+mgpp,j)
      end do
      enddo

c human readable/editable format (new)
      do j=1,2*jg
      do i=1,mg
      fliprap2(i,j)=rap2(i,2*jg+1-j)
      end do
      end do

c save files
      do j=1,2*jg
      write (14,"(16f8.3)")(fliprap2(i,j),i=1,16)
      write (15,"(16f8.3)")(fliprap2(i,j),i=17,32)
      write (16,"(16f8.3)")(fliprap2(i,j),i=33,48)
      write (17,"(16f8.3)")(fliprap2(i,j),i=49,64)
      write (18,"(16f8.3)")(fliprap2(i,j),i=65,80)
      write (19,"(16f8.3)")(fliprap2(i,j),i=81,96)
      write (20,"(16f8.3)")(fliprap2(i,j),i=97,112)
      write (21,"(16f8.3)")(fliprap2(i,j),i=113,128)
      write (41,"(e16.8)")(rap2(i,j),i=1,mg)
      end do


      stop
      end
