c
c  section ssum
c
c  sum a vector v
c
      real function ssum(n,v,iv)
      implicit real (a-h,o-z)
c
      integer iv,j,n
      real v(n)
c
      ssum=0.
      do 10 j=1,n
	 ssum=ssum+v((j-1)*iv+1)
   10 continue
c
      return
      end
