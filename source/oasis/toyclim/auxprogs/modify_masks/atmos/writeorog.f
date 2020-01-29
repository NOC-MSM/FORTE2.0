      parameter(mg=128,jg=32,mgpp=mg+2,igc=mgpp*2)
      parameter(nrunoff=18)

      integer listx(nrunoff),listy(nrunoff)

      integer map(mg,2*jg)
      integer flipmap(mg,2*jg)

      real gsg(igc,jg)
      real gsg2(igc,jg)
      real rap(mg,2*jg)
      real rap2(mg,2*jg)
      real fliprap2(mg,2*jg)
      character*2 vers

      data listx/88,88,89,90,91,92,93,94,95,96,97
     1   ,98,99,100,101,101,100,100/

      data listy/22,23,24,24,25,26,27,27,27,28,28
     1,29,30,30,30,31,32,33/

      write(6,*) " input version number (e.g. 01) "
      read(5,"(a2)")vers

c read data from human-readable files
      do j=1,2*jg
      read (14,"(16f8.3)")(fliprap2(i,j),i=1,16)
      read (15,"(16f8.3)")(fliprap2(i,j),i=17,32)
      read (16,"(16f8.3)")(fliprap2(i,j),i=33,48)
      read (17,"(16f8.3)")(fliprap2(i,j),i=49,64)
      read (18,"(16f8.3)")(fliprap2(i,j),i=65,80)
      read (19,"(16f8.3)")(fliprap2(i,j),i=81,96)
      read (20,"(16f8.3)")(fliprap2(i,j),i=97,112)
      read (21,"(16f8.3)")(fliprap2(i,j),i=113,128)
      end do

c flip data
      do j=1,2*jg
      do i=1,mg
      rap(i,j)=fliprap2(i,2*jg+1-j)
      end do
      end do

c rearrange orography to crazy IGCM3 format
      do j=1,jg
      do i=1,mg
      gsg(i,j)=rap(i,2*jg+1-j)
      end do
      enddo

      do j=jg,1,-1
      do i=1,mg
      gsg(i+mgpp,j)=rap(i,j)
      end do
      enddo

c write data to new file (vers)
      open(59,file='t42.59.'//vers)

      write(59,*)((gsg(i,j),i=1,mg),j=1,jg),(adum,i=1,mg),
     $          ((gsg(i+mgpp,j),i=1,mg),j=jg,1,-1)

      close(59)


      stop
      end
