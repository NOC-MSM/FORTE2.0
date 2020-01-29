      parameter(mg=128,jg=32,mgpp=mg+2,igc=mgpp*2)
      parameter(nrunoff=18)

      integer listx(nrunoff),listy(nrunoff)

      integer map(mg,2*jg)
      integer flipmap(mg,2*jg)

      integer map2(mg,2*jg)
      integer flipmap2(mg,2*jg)

      real gsg(igc,jg)
      real gsg2(igc,jg)
      real rap(mg,2*jg)
      real rap2(mg,2*jg)
      real fliprap2(mg,2*jg)


      data listx/88,88,89,90,91,92,93,94,95,96,97
     1   ,98,99,100,101,101,100,100/

      data listy/22,23,24,24,25,26,27,27,27,28,28
     1,29,30,30,30,31,32,33/

c read original orog file
      open(59,file='t42.59.00')

      read(59,*)((gsg(i,j),i=1,mg),j=1,jg),(adum,i=1,mg),
     $          ((gsg(i+mgpp,j),i=1,mg),j=jg,1,-1)

      close(59)

c read new mask file
      open(59,file='t42.59')

      read(59,*)((gsg2(i,j),i=1,mg),j=1,jg),(adum,i=1,mg),
     $          ((gsg2(i+mgpp,j),i=1,mg),j=jg,1,-1)

      close(59)

c make sense of original mask and orography
      do j=1,jg
      do i=1,mg
      rap(i,2*jg+1-j)=gsg(i,j)
      end do
      enddo

      do j=jg,1,-1
      do i=1,mg
      rap(i,j)=gsg(i+mgpp,j)
      end do
      enddo

c do same for new mask
      do j=1,jg
      do i=1,mg
      rap2(i,2*jg+1-j)=gsg2(i,j)
      end do
      enddo

      do j=jg,1,-1
      do i=1,mg
      rap2(i,j)=gsg2(i+mgpp,j)
      end do
      enddo

c copy orography data. if there is none then write 999.
      do j=1,2*jg
      do i=1,mg
      if(rap2(i,j).ne.0)then
      if(rap(i,j).eq.0)then
      rap2(i,j)=999.
      else
      rap2(i,j)=rap(i,j)
      endif
      endif
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
      write (3,'(128I1)')(flipmap(i,j),i=1,mg)
      write (31,*)(rap(i,j),i=1,mg)
      end do

      do j=1,2*jg
      write (14,"(16f8.3)")(fliprap2(i,j),i=1,16)
      write (15,"(16f8.3)")(fliprap2(i,j),i=17,32)
      write (16,"(16f8.3)")(fliprap2(i,j),i=33,48)
      write (17,"(16f8.3)")(fliprap2(i,j),i=49,64)
      write (18,"(16f8.3)")(fliprap2(i,j),i=65,80)
      write (19,"(16f8.3)")(fliprap2(i,j),i=81,96)
      write (20,"(16f8.3)")(fliprap2(i,j),i=97,112)
      write (21,"(16f8.3)")(fliprap2(i,j),i=113,128)
      write (41,*)(rap2(i,j),i=1,mg)
      end do

      stop
      end
