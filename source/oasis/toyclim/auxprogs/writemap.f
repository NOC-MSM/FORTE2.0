      parameter(mg=128,jg=32,mgpp=mg+2,igc=mgpp*2)
  
      real gsg(igc,jg)

      open(59,file='t42.59')                 

      read(59,*)((gsg(i,j),i=1,mg),j=1,jg),(adum,i=1,mg), 
     $          ((gsg(i+mgpp,j),i=1,mg),j=jg,1,-1)

      close(59)


c      plot out values for central america wher we need to make changes:
c     do j=1,jg
      do j=jg-5,jg
         read (3,*)
      enddo

c     do j=1,jg
      do j=jg-5,jg
c        read (3,'(10f8.3)')(gsg(i,j),i=97,106)
      enddo
      gsg(100,29)=561.31
      gsg(100,30)=631.42
      gsg(101,30)=701.53
      gsg(102,25)=700.
      gsg(103,25)=900.
      gsg(104,25)=450.
      gsg(102,26)=0.
      gsg(103,26)=0.
      gsg(104,26)=0.

      open(8,file="edits.list")
      do ii=1,7000
      read(8,"(5x,i12,5x,i12,7x,i12,7x,i12)",end=19)i,j,inew,iold
      if(j.le.32)then
      if(inew.eq.0)then
      gsg(i,j)=0
      else
      gsg(i,j)=1
      endif
      else
      if(inew.eq.0)then
      gsg(i+mgpp,2*jg+1-j)=0
      else
      gsg(i+mgpp,2*jg+1-j)=1
      endif
      endif
      end do
      close(8)

19    open(59,file='t42.59.modified')                 

      write(59,*)((gsg(i,j),i=1,mg),j=1,jg),(adum,i=1,mg), 
     $          ((gsg(i+mgpp,j),i=1,mg),j=jg,1,-1)

      close(59)
  
      stop
      end


     
