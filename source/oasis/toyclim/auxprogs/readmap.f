      parameter(mg=128,jg=32,mgpp=mg+2,igc=mgpp*2)
      parameter(nrunoff=18)

      integer listx(nrunoff),listy(nrunoff)

      integer map(mg,2*jg)
      integer flipmap(mg,2*jg)
  
      real gsg(igc,jg)

      data listx/88,88,89,90,91,92,93,94,95,96,97
     1   ,98,99,100,101,101,100,100/

      data listy/22,23,24,24,25,26,27,27,27,28,28
     1,29,30,30,30,31,32,33/

      open(59,file='t42.59')                 

      read(59,*)((gsg(i,j),i=1,mg),j=1,jg),(adum,i=1,mg), 
     $          ((gsg(i+mgpp,j),i=1,mg),j=jg,1,-1)

      close(59)


      write(3,*) 'land mask looks like:'
c      plot out values for central america wher we need to make changes:
      do j=1,jg
c     do j=jg-5,jg
c        write (3,'(128I1)')(nint((gsg(i,j)/
c    $                      (gsg(i,j)+.1))),i=1,mg)
c    $                      (gsg(i,j)+.1))),i=97,106)
      do i=1,mg
      map(i,2*jg+1-j)=nint( gsg(i,j)/(gsg(i,j)+.1) )
      end do
      enddo

c     do j=1,jg
c     do j=jg-5,jg
c        write (3,'(15f8.3)')(gsg(i,j),i=97,111)
c     enddo

      do j=jg,1,-1
c        write (3,'(128I1)')(nint((gsg(i+mgpp,j)/
c    $                      (gsg(i+mgpp,j)+.1))),i=1,mg)
      do i=1,mg
      map(i,j)=nint( gsg(i+mgpp,j)/(gsg(i+mgpp,j)+.1) )
      end do
      enddo

      do j=1,2*jg
      do i=1,mg
      flipmap(i,j)=map(i,2*jg+1-j)
      end do
      end do

c     do j=2*jg,1,-1
      do j=1,2*jg
      write (3,'(128I1)')(flipmap(i,j),i=1,mg)
      do i=1,mg
      write (31,*)map(i,j)
      end do
      end do
      open(4,file="fort.3.safe")
      do j=1,2*jg
      write (4,'(128I1)')(flipmap(i,j),i=1,mg)
      end do
  
      do j=22,33
      write (3,'(128I1)')(flipmap(i,j),i=86,114)
      end do

      do j=1,nrunoff
      write(6,*)j,listx(j),listy(j),flipmap(listx(j),listy(j))
      if(flipmap(listx(j),listy(j)).eq.1)then
      flipmap(listx(j),listy(j))=10
      else
      flipmap(listx(j),listy(j))=9
      endif
      end do 
  
      do j=22,33
      write (3,'(128I1)')(flipmap(i,j),i=86,114)
      end do

      stop
      end


     
