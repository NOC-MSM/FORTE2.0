c pdk changes here
        deltaT = 0.001
#ifndef no_salt
        deltaS = 0.001 
#endif
        j=0
        do 140 i=-1,1
          do k=1,kmt(ic+i,jc)
              tb(k) = t(k,ic+i,jc+j,1,nm)
#ifndef no_salt
              sb(k) = t(k,ic+i,jc+j,2,nm)  
#endif
          enddo
          call state(tb,sb,rhob,kmt(ic+i,jc),0)
          do k=1,kmt(ic+i,jc)
              tb(k) = tb(k) + deltaT
          enddo
          call state(tb,sb,rhobt,kmt(ic+i,jc),0)
          do k=1,kmt(ic+i,jc)
            alphai(k,i,j) = (rhobt(k) - rhob(k))/deltaT
            tb(k) = tb(k) - deltaT
#ifndef no_salt
            sb(k) = sb(k) + deltaS 
#endif
          enddo
#ifndef no_salt
          call state(tb,sb,rhobt,kmt(ic+i,jc),0)
          do k=1,kmt(ic+i,jc)
            betai(k,i,j) = (rhobt(k) - rhob(k))/deltaS
          enddo  
#endif
 140    continue
        i=0
        j=-1
          do k=1,kmt(ic+i,jc)
              tb(k) = t(k,ic+i,jc+j,1,nm)
#ifndef no_salt
              sb(k) = t(k,ic+i,jc+j,2,nm)  
#endif
          enddo
          call state(tb,sb,rhob,kmt(ic+i,jc),0)
          do k=1,kmt(ic+i,jc)
              tb(k) = tb(k) + deltaT
          enddo
          call state(tb,sb,rhobt,kmt(ic+i,jc),0)
          do k=1,kmt(ic+i,jc)
            alphai(k,i,j) = (rhobt(k) - rhob(k))/deltaT
            tb(k) = tb(k) - deltaT
#ifndef no_salt
            sb(k) = sb(k) + deltaS 
#endif
          enddo
#ifndef no_salt
          call state(tb,sb,rhobt,kmt(ic+i,jc),0)
          do k=1,kmt(ic+i,jc)
            betai(k,i,j) = (rhobt(k) - rhob(k))/deltaS
          enddo  
#endif
        i=0
        j=1
          do k=1,kmt(ic+i,jc)
              tb(k) = t(k,ic+i,jc+j,1,nm)
#ifndef no_salt
              sb(k) = t(k,ic+i,jc+j,2,nm)  
#endif
          enddo
          call state(tb,sb,rhob,kmt(ic+i,jc),0)
          do k=1,kmt(ic+i,jc)
              tb(k) = tb(k) + deltaT
          enddo
          call state(tb,sb,rhobt,kmt(ic+i,jc),0)
          do k=1,kmt(ic+i,jc)
            alphai(k,i,j) = (rhobt(k) - rhob(k))/deltaT
            tb(k) = tb(k) - deltaT
#ifndef no_salt
            sb(k) = sb(k) + deltaS 
#endif
          enddo
#ifndef no_salt
          call state(tb,sb,rhobt,kmt(ic+i,jc),0)
          do k=1,kmt(ic+i,jc)
            betai(k,i,j) = (rhobt(k) - rhob(k))/deltaS
          enddo  
#endif
