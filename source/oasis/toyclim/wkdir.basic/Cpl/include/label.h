C
C -- label.h   18-08-95   Version 2.0beta   Author: Laurent Terray
C    *******   01-02-96   Version 2.0 : disparition of ctranam
C              07-08-96   Version 2.1 : rise of cfldlab dimension
C              25-09-96   Version 2.1 : rise of cfldlab length
C@
C@  Contents : variables related to labelling files and character variable
C@  --------
C@
C@ -- cfldlab : field label definition (1D)
C@
C@ -- cgrdnam : name for grid file
C@
C@ -- cmsknam : name for mask file
C@
C@ -- csurnam : name for surface file
C@
C@ -- cglonsuf : suffix name for longitude grid file locator
C@
C@ -- cglatsuf : suffix name for longitude latitude grid file locator
C@
C@ -- cmsksuf : suffix name for mask file locator
C@
C@ -- csursuf : suffix name for surface file locator
C@
C@ -- crednam : name for reduced gaussian grid masks file
C@
C     -------------------------------------------------------------------
C
      CHARACTER*32 cfldlab(35)
      CHARACTER*5 cgrdnam, cmsknam, csurnam, crednam
      CHARACTER*4 cglonsuf, cglatsuf, cmsksuf, csursuf
C
      COMMON / comlab / cfldlab
      COMMON / comsuf / cgrdnam, cmsknam, csurnam, crednam,
     $                  cglonsuf, cglatsuf, cmsksuf, csursuf
C
C     -------------------------------------------------------------------
