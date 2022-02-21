c
c web lung bifurcation interface
c
      implicit none

      character*10000 LineIn
      integer lenLineIn,IOStat,ichar,ierr,abc
      real pdiameter
      real plength 
      real rodiameter
      real rosubang 
      real roroc   
      real rossl   
      real lodiameter
      real losubang 
      real loroc   
      real lossl   
      real carroc  
      real rp, rd1, rd2, rc, blp, bld1, bld2, ang1, ang2, pi, r1s, r2s
      logical getreal,error,errorpass
      external getreal
c
c Read the Web Input
c
!       Call GetEnv('CONTENT_LENGTH', LineIn)
!       READ(LineIn,*) lenLineIn
! C Read the data from the html form into the
! C variable LineIn, a single character at a time.
! C 
!       DO ichar=1,lenLineIn
!         call fget(LineIn(ichar:ichar),ierr)
!       ENDDO
! c
! c Successful read. 
! c Increase length of string by terminating last value with & so it looks
! c like the others
! c
!       LineIn(lenLineIn+1:lenLineIn+1) = "&"
!       lenLineIn = lenLineIn + 1
c
      pi = 4.*atan(1.)
      abc=1
c
c show what we get
c
c     write(66,'(''Content-Type: text/plain'',/)')
c     write(66,*) 'Here is what the FORTRAN gets as input:'
c     write(66,*) LineIn(1:lenLineIn)
c     write(66,*) "lenLineIn = ",lenLineIn
c     stop
c
c pull out the variable values and assign them
c
      if(abc==1) then
!       (getreal(pdiameter ,"pdiameter" ,LineIn,lenLineIn).and.
!      +   getreal(plength   ,"plength"   ,LineIn,lenLineIn).and.
!      +   getreal(rodiameter,"rodiameter",LineIn,lenLineIn).and.
!      +   getreal(rosubang  ,"rosubang"  ,LineIn,lenLineIn).and.
!      +   getreal(roroc     ,"roroc"     ,LineIn,lenLineIn).and.
!      +   getreal(rossl     ,"rossl"     ,LineIn,lenLineIn).and.
!      +   getreal(lodiameter,"lodiameter",LineIn,lenLineIn).and.
!      +   getreal(losubang  ,"losubang"  ,LineIn,lenLineIn).and.
!      +   getreal(loroc     ,"loroc"     ,LineIn,lenLineIn).and.
!      +   getreal(lossl     ,"lossl"     ,LineIn,lenLineIn).and.
!      +   getreal(carroc    ,"carroc"    ,LineIn,lenLineIn)) then
            pdiameter=1.6
            lodiameter=1.4
            rodiameter=1.4
            carroc=0.14
            plength=1.5
            lossl=1.2
            rossl=1.2
            loroc=6.4
            roroc=6.4
            losubang=35
            rosubang=35
c
c translate input parameters into ones DYL needs
c
      Rp = pdiameter/2.0      ! =0.8     ! radius of parent branch
      Rd1= lodiameter/2.0     ! =0.73    ! radius of left daughter branch
      Rd2= rodiameter/2.0     ! =0.55    ! radius of right daughter branch
      rc = carroc             ! =0.2*Rd1 ! radius of curvature at carinar ridge
      blp= plength            ! =3.*(2.*Rp)     ! length of parent branch
      bld1=lossl              ! =(Rd1/Rp)*blp  ! length of left daughter branch
      bld2=rossl              ! =(Rd2/Rp)*blp  ! length of right daughter branch
      r1s=loroc               ! left radius of curvature
      r2s=roroc               ! right radius of curvature

      ang1=losubang*pi/180.   ! =(25./180.)*pi   ! left dividing angle
      ang2=rosubang*pi/180.   ! =(45./180.)*pi   ! right dividing angle
      open(unit=66, file='bifurc.wrl')
c write out debug information
c
c     write(66,'(''Content-type: text/html'',/)')
c     write(66,'(''<body><html><pre>'')')
c     write(66,*) 'Rp = ', Rp
c     write(66,*) 'Rd1 = ', Rd1
c     write(66,*) 'Rd2 = ', Rd2
c     write(66,*) 'rc = ', rc
c     write(66,*) 'blp = ', blp
c     write(66,*) 'bld1 = ', bld1
c     write(66,*) 'bld2 = ', bld2
c     write(66,*) 'r1s  = ', r1s
c     write(66,*) 'r2s  = ', r2s
c     write(66,*) 'ang1 = ', ang1
c     write(66,*) 'ang2 = ', ang2
c     write(66,'(''</pre></html></body>'')')
c     goto 999
c
c Check the input and write error messages if any
c
        call dybifurc(rp,rd1,rd2,rc,blp,bld1,bld2,ang1,ang2,r1s,r2s,
     +       pi,.true.,error)
        if(error) then
          write(66,'(''</pre></html></body>'')')
        else
        
c
c Write the VRML introductory information and generate the geometry
c
          write(66,'(''Content-type: x-world/x-vrml'',/)')
          call dybifurc(rp,rd1,rd2,rc,blp,bld1,bld2,ang1,ang2,r1s,r2s,
     +       pi,.false.,error)
        endif
      else
c
c write error message if missing or character input
c
        write(66,'(''Content-type: text/html'',/)')
        write(66,'(''<body><html><pre>'')')
        write(66,*) "Input error, hit back and try again"
        write(66,'(''</pre></html></body>'')')
        stop
      endif
  999 continue
      stop
      end
c
c ------------------------------------------------- writerr
c
      subroutine writerr(string)
c
c prints html header info if error occurs
c
      implicit none
c
      character*(*) string
      logical firstpass
      data firstpass/.true./
c
      if(firstpass) then
        firstpass=.false.
        write(66,'(''Content-type: text/html'',/)')
        write(66,'(''<body><html><pre>'')')
      endif
      write(66,*) 'Input error: "', string, '", hit back and try again'
c
      return
      end
c
c ------------------------------------------------ getreal
c
      function getreal(value,variable,LineIn,lenLineIn)
c
c get a real variable value from LineIn and set it to value
c
      implicit none
c
      character*(*) variable,LineIn
      integer lenLineIn,loc1,loc2
      real value
      logical getreal
c
c assume getreal will work
c
      getreal = .true.
c
c find location of variable in LineIn
c
      loc1 = index(LineIn(1:lenLineIn),variable)
c     write(66,*) 'loc1 =',loc1
      if(loc1.eq.0) then
c       write(66,*) "variable not found: ",variable
        goto 991
      else
c
c find end of variable value
c
        loc2 = index(LineIn(loc1:lenLineIn),'&')
c       write(66,*) 'loc2 =',loc2
c
c show what we got so far
c
c       write(66,*) "value of ",variable," is ",
c    +     LineIn(loc1+len(variable)+1:loc1+loc2-2)
c
c internal read to convert characters into a number
c
        read(LineIn(loc1+len(variable)+1:loc1+loc2-2),*,err=991) value
      endif
      goto 1000
c
c not a number
c
  991 continue
      getreal = .false.
c     write(66,*) 'Not a number in variable ',variable
c
c all done
c
 1000 return
      end
c
c -----------------------------------------------------------------dybifurc
c
      SUBROUTINE dybifurc(Rp,Rd1,Rd2,rc,blpst,bld1st,bld2st,
     &                    ang1,ang2,R1s,R2s,pi,
     &                    errorpass,error)
c

c-------------- some array ---------
      DIMENSION del(5000),xx(5000),zz(5000) ! must be equal to nb
      DIMENSION xyz1(3), xyz2(3), xyz3(3)   ! three nodes for a face
      DIMENSION ff_u(100),ff_d(100)         ! store HEIGHT
      DIMENSION mar_u(100),mar_d(100)       ! 0:h<R2', 1:h>R2'

      logical errorpass,error
c-------
      error = .false.

c------- Open File -----------------
c     open(5,file='one.wrl',status='unknown')
c     open(7,file='up.dat',status='unknown')     !  check
c     open(9,file='inter.dat',status='unknown')  
c     open(11,file='dn.dat',status='unknown')    !  check
c     open(13,file='left.dat',status='unknown')
c     open(15,file='right.dat',status='unknown')

c------- move some variables into subroutine
      dv=1.0       ! dividing line setting     ! add
      btr=1.0      ! set the triangle shape near carina(btr=1:original)
      np=1    ! axial # of faces for straight parent region
      nt1=15  ! axial # of faces for transition zone(phi is btw 0 & alps)
      nt11=10 ! axial # of faces for transition zone(phi is btw 0 & alps) - tail
      nt2=10  ! axial # of faces for transition zone(phi is btw alps & alp)
      ndc=5   ! axial # of faces for daughter branch (curved zone)
      nd=1    ! axial # of faces for daughter branch (straight zone)
      na=40   ! angular # of facese for left or right hand side (upper), na/2 = even
      nb=5000 ! set the accuracy of pointB


c------- Set Variables to get VRML --------
      o90=90./180.*pi
      o180=180./180.*pi
      o270=270./180.*pi
      o360=360./180.*pi

      s1=Rp-Rd1
      s2=Rp-Rd2

      phi1s=ACOS( ((R1s+Rd1)**2-(R2s+Rd2)**2+(R1s+R2s+s1+s2)**2)/        ! left dividing angle at carinar ridge
     &            (2.*(R1s+R2s+s1+s2)*(R1s+Rd1)) )
      phi2s=ACOS( ((R2s+Rd2)**2-(R1s+Rd1)**2+(R1s+R2s+s1+s2)**2)/        ! left dividing angle at carinar ridge
     &            (2.*(R1s+R2s+s1+s2)*(R2s+Rd2)) )


c------- Set Geometric Variables for Transition Zone ----
      A=R1s+Rd1; B=R2s+Rd2
      alp1= ACOS( (R1s+R2s+s1+s2)/2./(A+rc)+                   ! angle at contact point of
     &      ((A+rc)**2-(B+rc)**2)/2./(A+rc)/(R1s+R2s+s1+s2) )  ! cainar curve and daughter branch
      alp2= ACOS( (R1s+R2s+s1+s2)/2./(B+rc)+                   
     &      ((B+rc)**2-(A+rc)**2)/2./(B+rc)/(R1s+R2s+s1+s2) )

      x1=(R1s+Rd1)*COS(alp1)-R1s-s1   ! coord. of contact pt of carinar circle and daughter branch
      z1=-(R1s+Rd1)*SIN(alp1)
      x2=R2s+s2-(R2s+Rd2)*COS(alp2)
      z2=-(R2s+Rd2)*SIN(alp2)
      xc=x1+COS(alp1)*rc           ! coord. of center of the carinar circle
      zc=z1-SIN(alp1)*rc

      do i=1,nb                     !! Find out the phi(alp1s, alp2s) (pointB) !!
       aa1=float(i)*(alp1/float(nb))
       aa2=(phi2s/phi1s)*aa1
       xx(i)=(-TAN(aa1)*(R1s+s1)+TAN(aa2)*(R2s+s2))/(TAN(aa1)+TAN(aa2))
       zz(i)=-TAN(aa1)*TAN(aa2)*(R1s+R2s+s1+s2)/(TAN(aa1)+TAN(aa2)) 
       del(i)=ABS((xx(i)-xc)**2+(zz(i)-zc)**2-rc**2)
      enddo
      amin=1000.
      do i=1,nb
       if(del(i).le.amin) then
        xxb=xx(i); zzb=zz(i); amin=del(i)
       endif
      enddo                         
      alp1s=ATAN(-zzb/(xxb+R1s+s1))       
      alp2s=alp1s*(phi2s/phi1s)     !! Find out the phi(alp1s, alp2s) (pointB) !!

      IF(ang1.lt.alp1) THEN         !! error message
c        print *,'ang1 is too small or R1s is too small'
        if(errorpass) then
          call writerr('ang1 is too small or R1s is too small')
          error = .true.
        endif
      ENDIF
      IF(ang2.lt.alp2) THEN
c        print *,'ang2 is too small or R2s is too small'
        if(errorpass) then
          call writerr('ang2 is too small or R2s is too small')
          error = .true.
        endif
      ENDIF


c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c                   Main Start
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c--------------------------------------------------
c     write header 
c--------------------------------------------------
      call header('bifurcation',errorpass)

c--------------------------------------------------
c     parent branch (straight) 
c--------------------------------------------------
      IF(blpst.gt.0.0) THEN 

c++++++++++ left +++++++++++++++++++++++++
      delR=Rp-Rd1
      th=asin((Rp-Rd1)/Rp)
      alx=(s1+s2)/2.

      tmp1=float(na/2)*(alx/(alx+Rd1))
      tmp2=INT(tmp1)
      if((tmp1-tmp2).ge.0.5) then              ! number of face at inner transition zone
       nn=tmp2+1
      else
       nn=tmp2
      endif
      if(nn.eq.0) nn=1

      do j=1,np               ! axial loop
        z=blpst-blpst*float(j-1)/float(np)
        delz=blpst/float(np)

        do i=1,na/2              ! angular loop
          theta=o90+th+(o90-th)*float(i-nn-1)/float(na/2-nn)
          delth=(o90-th)/float(na/2-nn)
          xs=alx*cos((i-1)*o90/nn)
          delx=alx*( cos((i-1)*o90/nn)-cos(i*o90/nn) )

c-------- first triangle
          if(i.eq.1) then              
           xyz1(1)=(-s1+s2)/2.
           xyz1(2)=sqrt(Rp**2-(-s1+s2)**2/2.**2)
           xyz1(3)=z
          endif                                     
          if(i.gt.1) then   
           xyz1(1)=oldx3
           xyz1(2)=oldy3
           xyz1(3)=oldz3
          endif

          if(i+1.gt.nn) then 
           xyz3(1)=Rp*COS(theta+delth)
           xyz3(2)=Rp*SIN(theta+delth)
           xyz3(3)=z
          endif
          if(i+1.le.nn) then 
           xyz3(1)=(xs-delx)-delR
           xyz3(2)=sqrt(Rp**2-(xs-delx-delR)**2)
           xyz3(3)=z
          endif     

          if(i+1.gt.nn) then    
           xyz2(1)=Rp*COS(theta+delth)
           xyz2(2)=Rp*SIN(theta+delth)
           xyz2(3)=z-delz
          endif
          if(i+1.le.nn) then 
           xyz2(1)=(xs-delx)-delR
           xyz2(2)=sqrt(Rp**2-(xs-delx-delR)**2)
           xyz2(3)=z-delz
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          oldx2=tx2; oldy2=ty2; oldz2=tz2      

          tx1=xyz1(1); ty1=xyz1(2); tz1=xyz1(3)
          tx2=xyz2(1); ty2=xyz2(2); tz2=xyz2(3)
          tx3=xyz3(1); ty3=xyz3(2); tz3=xyz3(3)

          oldx3=tx3; oldy3=ty3; oldz3=tz3       

c-------- second triangle
          xyz1(1)=tx1
          xyz1(2)=ty1
          xyz1(3)=tz1
          xyz3(1)=tx2
          xyz3(2)=ty2
          xyz3(3)=tz2

          if(i.eq.1) then 
           xyz2(1)=xs-delR
           xyz2(2)=sqrt(Rp**2-(xs-delR)**2)
           xyz2(3)=z-delz
          endif
          if(i.gt.1) then           
           xyz2(1)=oldx2
           xyz2(2)=oldy2
           xyz2(3)=oldz2
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx4=xyz2(1); ty4=xyz2(2); tz4=xyz2(3)   ! for opposite faces

c-------- opposite faces
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx3; xyz2(2)=-ty3; xyz2(3)=tz3
           xyz3(1)=tx2; xyz3(2)=-ty2; xyz3(3)=tz2
           call write_facet(xyz1,xyz2,xyz3,errorpass)
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx2; xyz2(2)=-ty2; xyz2(3)=tz2
           xyz3(1)=tx4; xyz3(2)=-ty4; xyz3(3)=tz4
           call write_facet(xyz1,xyz2,xyz3,errorpass)

        enddo              ! angular loop end
      enddo                   ! axial loop end

c++++++++++ right ++++++++++++++++++++++++
      delR=Rp-Rd2
      th=asin((Rp-Rd2)/Rp)
      alx=(s1+s2)/2.

      tmp1=float(na/2)*(alx/(alx+Rd2))
      tmp2=INT(tmp1)
      if((tmp1-tmp2).ge.0.5) then              ! number of face at inner transition zone
       nn=tmp2+1
      else
       nn=tmp2
      endif
      if(nn.eq.0) nn=1

      do j=1,np               ! axial loop
        z=blpst-blpst*float(j-1)/float(np)
        delz=blpst/float(np)

        do i=1,na/2              ! angular loop
          theta=o90-th-(o90-th)*float(i-nn-1)/float(na/2-nn)
          delth=(o90-th)/float(na/2-nn)
          xs=alx*cos((i-1)*o90/nn)
          delx=alx*( cos((i-1)*o90/nn)-cos(i*o90/nn) )

c-------- first triangle
          if(i.eq.1) then
           xyz1(1)=(-s1+s2)/2.
           xyz1(2)=sqrt(Rp**2-(-s1+s2)**2/2.**2)
           xyz1(3)=z
          endif
          if(i.gt.1) then
           xyz1(1)=oldx2
           xyz1(2)=oldy2
           xyz1(3)=oldz2
          endif

          if(i+1.gt.nn) then
           xyz2(1)=Rp*COS(theta-delth)
           xyz2(2)=Rp*SIN(theta-delth)
           xyz2(3)=z
          endif
          if(i+1.le.nn) then
           xyz2(1)=(delR-(xs-delx))
           xyz2(2)=sqrt(Rp**2-(delR-(xs-delx))**2)
           xyz2(3)=z
          endif

          if(i+1.gt.nn) then
           xyz3(1)=Rp*COS(theta-delth)
           xyz3(2)=Rp*SIN(theta-delth)
           xyz3(3)=z-delz
          endif
          if(i+1.le.nn) then
           xyz3(1)=(delR-(xs-delx))
           xyz3(2)=sqrt(Rp**2-(delR-(xs-delx))**2)
           xyz3(3)=z-delz
          endif

          oldx3=tx3; oldy3=ty3; oldz3=tz3

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx1=xyz1(1); ty1=xyz1(2); tz1=xyz1(3)
          tx2=xyz2(1); ty2=xyz2(2); tz2=xyz2(3)
          tx3=xyz3(1); ty3=xyz3(2); tz3=xyz3(3)

          oldx2=tx2; oldy2=ty2; oldz2=tz2

c-------- second triangle
          xyz1(1)=tx1
          xyz1(2)=ty1
          xyz1(3)=tz1
          xyz2(1)=tx3
          xyz2(2)=ty3
          xyz2(3)=tz3

          if(i.eq.1) then
           xyz3(1)=(delR-xs)
           xyz3(2)=sqrt(Rp**2-(delR-xs)**2)
           xyz3(3)=z-delz
          endif
          if(i.gt.1) then
           xyz3(1)=oldx3
           xyz3(2)=oldy3
           xyz3(3)=oldz3
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx4=xyz3(1); ty4=xyz3(2); tz4=xyz3(3)   ! for opposite faces

c-------- opposite faces
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx3; xyz2(2)=-ty3; xyz2(3)=tz3
           xyz3(1)=tx2; xyz3(2)=-ty2; xyz3(3)=tz2
           call write_facet(xyz1,xyz2,xyz3,errorpass)
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx4; xyz2(2)=-ty4; xyz2(3)=tz4
           xyz3(1)=tx3; xyz3(2)=-ty3; xyz3(3)=tz3
           call write_facet(xyz1,xyz2,xyz3,errorpass)

        enddo                 ! angular loop end
      enddo                   ! axial loop end

      ENDIF


c--------------------------------------------------
c      upper transition (0 < phi < alps) 
c--------------------------------------------------

      CALL HEIGHT(R1s,R2s,Rp,Rd1,Rd2,alp1s,alp2s,phi1s,phi2s,
     &     j,nt1,nt11,ff_u,ff_d,zzb,mar_u,mar_d,
     &     bt1l,bt1r,bt2l,bt2r,dv,s1,s2)

c      print *,bt1l*180./pi,bt2l*180./pi,alp1s*180./pi
c      print *,''

c++++++++++++ left ++++++++++++++++++++++++

      do j=1,nt1-1+nt11               ! axial loop (left)

        if(j.le.nt1-1) then
         phi_u=alp1s*float(j-1)/float(nt1)   ! upper of a face
         phi_d=alp1s*float(j)/float(nt1)     ! lower of a face
        else
         phi_u=alp1s*float(nt1-1)/float(nt1)
     &        +float(j-nt1)*alp1s/float(nt1)/float(nt11)
         phi_d=alp1s*float(nt1-1)/float(nt1)
     &        +float(j-nt1+1)*alp1s/float(nt1)/float(nt11)
        endif

        CALL VAR1(R1s,R2s,Rp,Rd1,Rd2,na,pi,alp1s,phi_u,phi_d,phi1s,
     &  phi2s,alx_u,alx_d,n_u,n_d,R1_u,R1_d,zzb,xxb,nt1,nt11,j,dv,
     &  s1,s2)

        if(j.le.nt1-1) then                                             ! for FFL
         c_u=alp1s*float(j-1)/float(nt1)                                ! consider
         c_d=alp1s*float(j)/float(nt1)                                  ! dividing curve
         t_u=-2.*(c_u/alp1s)**3+3.*(c_u/alp1s)**2
         t_d=-2.*(c_d/alp1s)**3+3.*(c_d/alp1s)**2
         phi_u2=t_u**dv*c_u*phi2s/phi1s            
     &  +(1.-t_u**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c_u))
         phi_d2=t_d**dv*c_d*phi2s/phi1s
     &  +(1.-t_d**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c_d))
        else
         c_u=alp1s*float(nt1-1)/float(nt1)
     &        +float(j-nt1)*alp1s/float(nt1)/float(nt11)
         c_d=alp1s*float(nt1-1)/float(nt1)
     &        +float(j-nt1+1)*alp1s/float(nt1)/float(nt11)
         t_u=-2.*(c_u/alp1s)**3+3.*(c_u/alp1s)**2
         t_d=-2.*(c_d/alp1s)**3+3.*(c_d/alp1s)**2
         phi_u2=t_u**dv*c_u*phi2s/phi1s
     &  +(1.-t_u**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c_u))
         phi_d2=t_d**dv*c_d*phi2s/phi1s
     &  +(1.-t_d**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c_d))
        endif

        CALL VAR2(R1s,R2s,Rp,Rd1,Rd2,na,pi,alp1s,alp2s,
     &  phi_u2,phi_d2,phi1s,
     &  phi2s,alx_u2,alx_d2,n_u2,n_d2,R2_u,R2_d,zzb,xxb,nt1,nt11,j,dv,
     &  s1,s2)                                                           ! for FFL

        fy_u=ff_u(j); fy_d=ff_d(j)

        dR1_u=R1_u-Rd1
        dR1_d=R1_d-Rd1
        dR2_u=R2_u-Rd2
        dR2_d=R2_d-Rd2


        if(j.eq.nt1-1+nt11) num_l=n_d   ! this is only for better showing

        tt=(phi_u-bt1l)/(bt2l-bt1l)     ! 0-1 for interpolation
        w_u=-2.*tt**3+3.*tt**2
c        w_u=tt**1.0
        tt=(phi_d-bt1l)/(bt2l-bt1l)
        w_d=-2.*tt**3+3.*tt**2
c        w_d=tt**1.0
        if(phi_u.ge.bt2l) w_u=1.0
        if(phi_d.ge.bt2l) w_d=1.0       !     for interpolation

        th_u=asin((R1_u-Rd1)/R1_u)
        th_d=asin((R1_d-Rd1)/R1_d)

        do i=1,na/2              ! angular loop
          theta_u=o90+th_u+(o90-th_u)*float(i-n_u-1)/float(na/2-n_u)
          theta_d=o90+th_d+(o90-th_d)*float(i-n_d-1)/float(na/2-n_d)
          delth_u=(o90-th_u)/float(na/2-n_u)
          delth_d=(o90-th_d)/float(na/2-n_d)
          xs_u=alx_u*cos((i-1)*o90/n_u)
          xs_d=alx_d*cos((i-1)*o90/n_d)
          delx_u=alx_u*( cos((i-1)*o90/n_u)-cos(i*o90/n_u) )
          delx_d=alx_d*( cos((i-1)*o90/n_d)-cos(i*o90/n_d) )

          xs_d2=alx_d-alx_d*float(i-1)/float(n_d)  ! for better showing
          delx_d2=alx_d/float(n_d)
          xs_d2=btr*xs_d+(1-btr)*xs_d2
          delx_d2=btr*delx_d+(1-btr)*delx_d2

c-------- first triangle
          if(i.eq.1.and.i.le.n_u) then                   ! if node1 in the i=1 triangle
           xyz1(1)=(R1s+xs_u)*COS(phi_u)-R1s-s1
           if(mar_u(j).eq.0) then
            aa1=F1l(Rp,phi_u,R1_u,fy_u,alx_u,xs_u,j,nt1,nt11,1,dR1_u)
            aa2=FFL(Rp,phi_u,phi_u2,R1_u,R2_u,fy_u,alx_u,alx_u2,xs_u,j,
     &              dR1_u,dR2_u) 
            xyz1(2)=aa1*w_u + aa2*(1.-w_u)
           else
            xyz1(2)=FFL(Rp,phi_u,phi_u2,R1_u,R2_u,fy_u,alx_u,alx_u2,
     &                  xs_u,j,dR1_u,dR2_u)
           endif
           xyz1(3)=-(R1s+xs_u)*SIN(phi_u)
          endif                        
          if(i.eq.1.and.j.eq.1) then                  ! model2        
           xyz1(1)=(-s1+s2)/2.
           xyz1(2)=sqrt(Rp**2-(-s1+s2)**2/2.**2)
           xyz1(3)=0.0
          endif                                       ! model2
          if(i.gt.1) then                                ! use node3 of previous triangle
           xyz1(1)=oldx3
           xyz1(2)=oldy3
           xyz1(3)=oldz3
          endif
          if(j.eq.3) then  ! check
c           write(13,*) xyz1(1),xyz1(2)
          endif

          if(j.eq.nt1-1+nt11.and.i.le.n_u) then        !!!!!!!!!!!!!!! 
           if(i.eq.1) then                             !             !
            xp1=xyz1(1);yp1=xyz1(2);zp1=xyz1(3)        !             !
           endif                                       !             !
           po=sqrt((xp1-xyz1(1))**2+(yp1-xyz1(2))**2   !     check   !
     &             +(zp1-xyz1(3))**2)                  !             !
           po=sqrt(po**2-(xyz1(2)-yp1)**2)             !             !
c          write(7,*) alx_u-po,xyz1(2)                 !             !
          endif                                        !!!!!!!!!!!!!!!

          if(i+1.le.n_u) then                 ! node3 is in inner zone : y(+) 
           xyz3(1)=(R1s+(xs_u-delx_u))*COS(phi_u)-R1s-s1
           if(mar_u(j).eq.0) then
            aa1=F1l(Rp,phi_u,R1_u,fy_u,alx_u,xs_u-delx_u,j,nt1,nt11,1,
     &              dR1_u)
            aa2=FFL(Rp,phi_u,phi_u2,R1_u,R2_u,fy_u,alx_u,alx_u2,
     &              xs_u-delx_u,j,dR1_u,dR2_u)
            xyz3(2)=aa1*w_u + aa2*(1.-w_u)
           else
            xyz3(2)=FFL(Rp,phi_u,phi_u2,R1_u,R2_u,fy_u,alx_u,alx_u2,
     &                  xs_u-delx_u,j,dR1_u,dR2_u)
           endif
           xyz3(3)=-(R1s+(xs_u-delx_u))*SIN(phi_u)
          endif                          
          if(i+1.gt.n_u) then                 ! node3 is in outer zone
           xyz3(1)=(R1s-Rd1+R1_u+R1_u*COS(theta_u+delth_u))*COS(phi_u)
     &             -R1s-s1
           xyz3(2)=R1_u*SIN(theta_u+delth_u)
           xyz3(3)=-(R1s-Rd1+R1_u+R1_u*COS(theta_u+delth_u))*SIN(phi_u)
          endif 
          if(i+1.le.n_u.and.j.eq.1) then                            ! model2
           xyz3(1)=(R1s+(xs_u-delx_u))*COS(phi_u)-R1s-s1
           xyz3(2)=sqrt(Rp**2-(xs_u-delx_u-(dR1_u))**2)
           xyz3(3)=-(R1s+(xs_u-delx_u))*SIN(phi_u)
          endif                                                     ! model2

          if(i+1.le.n_d) then                 ! node2 is in inner zone : y(+)
           IF(j.ne.nt1-1+nt11) THEN           ! for better showing
            xyz2(1)=(R1s+(xs_d-delx_d))*COS(phi_d)-R1s-s1
            if(mar_d(j).eq.0) then
             aa1=F1l(Rp,phi_d,R1_d,fy_d,alx_d,xs_d-delx_d,j,nt1,nt11,2,
     &               dR1_d)
             aa2=FFL(Rp,phi_d,phi_d2,R1_d,R2_d,fy_d,alx_d,alx_d2,
     &                   xs_d-delx_d,j,dR1_d,dR2_d)
             xyz2(2)=aa1*w_d + aa2*(1.-w_d)
            else
             xyz2(2)=FFL(Rp,phi_d,phi_d2,R1_d,R2_d,fy_d,alx_d,alx_d2,
     &                   xs_d-delx_d,j,dR1_d,dR2_d)
            endif
            xyz2(3)=-(R1s+(xs_d-delx_d))*SIN(phi_d)
           ELSE
            xyz2(1)=(R1s+(xs_d2-delx_d2))*COS(phi_d)-R1s-s1
            if(mar_d(j).eq.0) then
             aa1=F1l(Rp,phi_d,R1_d,fy_d,alx_d,xs_d2-delx_d2,
     &               j,nt1,nt11,2,dR1_d)
             aa2=FFL(Rp,phi_d,phi_d2,R1_d,R2_d,fy_d,alx_d,alx_d2,
     &                   xs_d2-delx_d2,j,dR1_d,dR2_d)
             xyz2(2)=aa1*w_d + aa2*(1.-w_d)
            else
             xyz2(2)=FFL(Rp,phi_d,phi_d2,R1_d,R2_d,fy_d,alx_d,alx_d2,
     &                   xs_d2-delx_d2,j,dR1_d,dR2_d)
            endif
            xyz2(3)=-(R1s+(xs_d2-delx_d2))*SIN(phi_d) 
           ENDIF                              ! for better showing
          else                                ! node2 is in outer zone 
           xyz2(1)=(R1s-Rd1+R1_d+R1_d*COS(theta_d+delth_d))*COS(phi_d)
     &             -R1s-s1
           xyz2(2)=R1_d*SIN(theta_d+delth_d)
           xyz2(3)=-(R1s-Rd1+R1_d+R1_d*COS(theta_d+delth_d))*SIN(phi_d)
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          oldx2=tx2; oldy2=ty2; oldz2=tz2         ! previous triangle

          tx1=xyz1(1); ty1=xyz1(2); tz1=xyz1(3)   ! for opposite faces
          tx2=xyz2(1); ty2=xyz2(2); tz2=xyz2(3)  
          tx3=xyz3(1); ty3=xyz3(2); tz3=xyz3(3)

          oldx3=tx3; oldy3=ty3; oldz3=tz3         ! previous triangle


c-------- second triangle
           xyz1(1)=tx1
           xyz1(2)=ty1
           xyz1(3)=tz1
           xyz3(1)=tx2
           xyz3(2)=ty2
           xyz3(3)=tz2
          if(i.eq.1.and.i.le.n_d) then                  ! node2 in the i=1 triangle
           IF(j.ne.nt1-1+nt11) THEN                     ! for better showing
            xyz2(1)=(R1s+xs_d)*COS(phi_d)-R1s-s1
            if(mar_d(j).eq.0) then
             aa1=F1l(Rp,phi_d,R1_d,fy_d,alx_d,xs_d,j,nt1,nt11,2,dR1_d)
             aa2=FFL(Rp,phi_d,phi_d2,R1_d,R2_d,fy_d,alx_d,alx_d2,xs_d,j,
     &               dR1_d,dR2_d)
             xyz2(2)=aa1*w_d + aa2*(1.-w_d)
            else
             xyz2(2)=FFL(Rp,phi_d,phi_d2,R1_d,R2_d,fy_d,alx_d,alx_d2,
     &                   xs_d,j,dR1_d,dR2_d)
            endif
            xyz2(3)=-(R1s+xs_d)*SIN(phi_d)
           ELSE
            xyz2(1)=(R1s+xs_d2)*COS(phi_d)-R1s-s1
            if(mar_d(j).eq.0) then
             aa1=F1l(Rp,phi_d,R1_d,fy_d,alx_d,xs_d2,j,nt1,nt11,2,dR1_d)
             aa2=FFL(Rp,phi_d,phi_d2,R1_d,R2_d,fy_d,alx_d,
     &               alx_d2,xs_d2,j,dR1_d,dR2_d)
             xyz2(2)=aa1*w_d + aa2*(1.-w_d)
            else
             xyz2(2)=FFL(Rp,phi_d,phi_d2,R1_d,R2_d,fy_d,alx_d,alx_d2,
     &                   xs_d2,j,dR1_d,dR2_d)
            endif
            xyz2(3)=-(R1s+xs_d2)*SIN(phi_d)
           ENDIF                                         ! for better showing
          endif
          if(i.eq.1.and.i.gt.n_d) then
           xyz2(1)=(R1s-Rd1+R1_d+R1_d*COS(theta_d))*COS(phi_d)-R1s-s1
           xyz2(2)=R1_d*SIN(theta_d)
           xyz2(3)=-(R1s-Rd1+R1_d+R1_d*COS(theta_d))*SIN(phi_d)
          endif
          if(i.gt.1) then                     ! use node2 of previous triangle
           xyz2(1)=oldx2
           xyz2(2)=oldy2
           xyz2(3)=oldz2
          endif
          if(j.eq.nt1-1+nt11.and.i.le.n_d) then        !!!!!!!!!!!!!!!
           if(i.eq.1) then                             !             !
            xp2=xyz2(1);yp2=xyz2(2);zp2=xyz2(3)        !             !
           endif                                       !             !
           po=sqrt((xp2-xyz2(1))**2+(yp2-xyz2(2))**2   !     check   !
     &             +(zp2-xyz2(3))**2)                  !             !
           po=sqrt(po**2-xyz2(2)**2)                   !             !
c          write(9,*) alx_d-po,xyz2(2)                 !             !
          endif                                        !!!!!!!!!!!!!!!

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx4=xyz2(1); ty4=xyz2(2); tz4=xyz2(3)   ! for opposite faces

c-------- opposite faces
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx3; xyz2(2)=-ty3; xyz2(3)=tz3
           xyz3(1)=tx2; xyz3(2)=-ty2; xyz3(3)=tz2
           call write_facet(xyz1,xyz2,xyz3,errorpass)
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx2; xyz2(2)=-ty2; xyz2(3)=tz2
           xyz3(1)=tx4; xyz3(2)=-ty4; xyz3(3)=tz4
           call write_facet(xyz1,xyz2,xyz3,errorpass)

        enddo              ! angular loop end
      enddo                ! axial loop end


c++++++++++++ right ++++++++++++++++++++++++
      do j=1,nt1-1+nt11               ! axial loop (right)

        if(j.le.nt1-1) then
         c_u=alp1s*float(j-1)/float(nt1)                                ! consider
         c_d=alp1s*float(j)/float(nt1)                                  ! dividing curve
         t_u=-2.*(c_u/alp1s)**3+3.*(c_u/alp1s)**2
         t_d=-2.*(c_d/alp1s)**3+3.*(c_d/alp1s)**2
         phi_u=t_u**dv*c_u*phi2s/phi1s
     &  +(1.-t_u**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c_u))
         phi_d=t_d**dv*c_d*phi2s/phi1s
     &  +(1.-t_d**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c_d))
        else
         c_u=alp1s*float(nt1-1)/float(nt1)
     &        +float(j-nt1)*alp1s/float(nt1)/float(nt11)
         c_d=alp1s*float(nt1-1)/float(nt1)
     &        +float(j-nt1+1)*alp1s/float(nt1)/float(nt11)
         t_u=-2.*(c_u/alp1s)**3+3.*(c_u/alp1s)**2
         t_d=-2.*(c_d/alp1s)**3+3.*(c_d/alp1s)**2
         phi_u=t_u**dv*c_u*phi2s/phi1s
     &  +(1.-t_u**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c_u))
         phi_d=t_d**dv*c_d*phi2s/phi1s
     &  +(1.-t_d**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c_d))
        endif

        CALL VAR2(R1s,R2s,Rp,Rd1,Rd2,na,pi,alp1s,alp2s,
     &  phi_u,phi_d,phi1s,
     &  phi2s,alx_u,alx_d,n_u,n_d,R2_u,R2_d,zzb,xxb,nt1,nt11,j,dv,
     &  s1,s2)

        fy_u=ff_u(j); fy_d=ff_d(j)

        if(j.le.nt1-1) then                                             ! for FFR
         phi_u1=alp1s*float(j-1)/float(nt1)
         phi_d1=alp1s*float(j)/float(nt1)
        else
         phi_u1=alp1s*float(nt1-1)/float(nt1)
     &        +float(j-nt1)*alp1s/float(nt1)/float(nt11)
         phi_d1=alp1s*float(nt1-1)/float(nt1)
     &        +float(j-nt1+1)*alp1s/float(nt1)/float(nt11)
        endif

        CALL VAR1(R1s,R2s,Rp,Rd1,Rd2,na,pi,alp1s,phi_u1,phi_d1,phi1s,
     &  phi2s,alx_u1,alx_d1,n_u1,n_d1,R1_u,R1_d,zzb,xxb,nt1,nt11,j,dv,
     &  s1,s2)                                                           ! for FFR

        if(j.eq.nt1-1+nt11) num_r=n_d   ! this is only for better showing

        dR1_u=R1_u-Rd1
        dR1_d=R1_d-Rd1
        dR2_u=R2_u-Rd2
        dR2_d=R2_d-Rd2

        tt=(phi_u1-bt1l)/(bt2l-bt1l)     ! 0-1 for interpolation
        w_u=-2.*tt**3+3.*tt**2
c        w_u=tt**1.0
        tt=(phi_d1-bt1l)/(bt2l-bt1l)
        w_d=-2.*tt**3+3.*tt**2
c        w_d=tt**1.0
        if(phi_u1.ge.bt2l) w_u=1.0
        if(phi_d1.ge.bt2l) w_d=1.0       !     for interpolation

        th_u=asin((R2_u-Rd2)/R2_u)
        th_d=asin((R2_d-Rd2)/R2_d)

        do i=1,na/2                    ! angular loop
          theta_u=o90+th_u+(o90-th_u)*float(i-n_u-1)/float(na/2-n_u)
          theta_d=o90+th_d+(o90-th_d)*float(i-n_d-1)/float(na/2-n_d)
          delth_u=(o90-th_u)/float(na/2-n_u)
          delth_d=(o90-th_d)/float(na/2-n_d)
          xs_u=alx_u*cos((i-1)*o90/n_u)
          xs_d=alx_d*cos((i-1)*o90/n_d)
          delx_u=alx_u*( cos((i-1)*o90/n_u)-cos(i*o90/n_u) )
          delx_d=alx_d*( cos((i-1)*o90/n_d)-cos(i*o90/n_d) )

          xs_d2=alx_d-alx_d*float(i-1)/float(n_d)   ! for better showing
          delx_d2=alx_d/float(n_d)
          xs_d2=btr*xs_d+(1-btr)*xs_d2
          delx_d2=btr*delx_d+(1-btr)*delx_d2


c-------- first triangle
          if(i.eq.1.and.i.le.n_u) then      ! node1 in the i=1 triangle
           xyz1(1)=R2s+s2-(R2s+xs_u)*COS(phi_u)
           if(mar_u(j).eq.0) then
            aa1=F1r(Rp,phi_u,R2_u,fy_u,alx_u,xs_u,j,nt1,nt11,1,dR2_u)
            aa2=FFR(Rp,phi_u,phi_u1,R2_u,R1_u,fy_u,alx_u,alx_u1,xs_u,
     &              dR1_u,dR2_u)
            xyz1(2)=aa1*w_u + aa2*(1.-w_u)
           else
            xyz1(2)=FFR(Rp,phi_u,phi_u1,R2_u,R1_u,fy_u,alx_u,alx_u1,
     &                  xs_u,dR1_u,dR2_u)
           endif
           xyz1(3)=-(R2s+xs_u)*SIN(phi_u)
          endif
          if(i.eq.1.and.j.eq.1) then                                 ! model2
           xyz1(1)=(-s1+s2)/2.
           xyz1(2)=sqrt(Rp**2-(-s1+s2)**2/2.**2)
           xyz1(3)=0.0
          endif                                                      ! model2
          if(i.gt.1) then                   ! use node2 of previous triangle
           xyz1(1)=oldx2
           xyz1(2)=oldy2
           xyz1(3)=oldz2
          endif
          if(j.eq.3) then  ! check
c          write(15,*) xyz1(1),xyz1(2)
          endif

          if(i+1.le.n_u) then               ! node2 is in inner zone : y(+)
           xyz2(1)=R2s+s2-(R2s+(xs_u-delx_u))*COS(phi_u)
           if(mar_u(j).eq.0) then
            aa1=F1r(Rp,phi_u,R2_u,fy_u,alx_u,xs_u-delx_u,j,nt1,nt11,1,
     &              dR2_u)
            aa2=FFR(Rp,phi_u,phi_u1,R2_u,R1_u,fy_u,alx_u,alx_u1,
     &                  xs_u-delx_u,dR1_u,dR2_u)
            xyz2(2)=aa1*w_u + aa2*(1.-w_u)
           else
            xyz2(2)=FFR(Rp,phi_u,phi_u1,R2_u,R1_u,fy_u,alx_u,alx_u1,
     &                  xs_u-delx_u,dR1_u,dR2_u)
           endif
           xyz2(3)=-(R2s+(xs_u-delx_u))*SIN(phi_u)
          endif
          if(i+1.gt.n_u) then               ! node2 is in outer zone
           xyz2(1)=R2s+s2
     &            -(R2s-Rd2+R2_u+R2_u*COS(theta_u+delth_u))*COS(phi_u)
           xyz2(2)=R2_u*SIN(theta_u+delth_u)
           xyz2(3)=-(R2s-Rd2+R2_u+R2_u*COS(theta_u+delth_u))*SIN(phi_u)
          endif
          if(i+1.le.n_u.and.j.eq.1) then                             ! model2
           xyz2(1)=R2s+s2-(R2s+(xs_u-delx_u))*COS(phi_u)
           xyz2(2)=sqrt(Rp**2-(dR2_u-(xs_u-delx_u))**2)
           xyz2(3)=-(R2s+(xs_u-delx_u))*SIN(phi_u)                   ! model2
          endif

          if(i+1.le.n_d) then               ! node3 is in inner zone : y(+)
           IF(j.ne.nt1-1+nt11) THEN       ! for better showing
            xyz3(1)=R2s+s2-(R2s+(xs_d-delx_d))*COS(phi_d)
            if(mar_d(j).eq.0) then
             aa1=F1r(Rp,phi_d,R2_d,fy_d,alx_d,xs_d-delx_d,j,nt1,nt11,2,
     &               dR2_d)
             aa2=FFR(Rp,phi_d,phi_d1,R2_d,R1_d,fy_d,alx_d,alx_d1,
     &                   xs_d-delx_d,dR1_d,dR2_d)
             xyz3(2)=aa1*w_d + aa2*(1.-w_d)
            else
             xyz3(2)=FFR(Rp,phi_d,phi_d1,R2_d,R1_d,fy_d,alx_d,alx_d1,
     &                   xs_d-delx_d,dR1_d,dR2_d)
            endif
            xyz3(3)=-(R2s+(xs_d-delx_d))*SIN(phi_d)
           ELSE
            xyz3(1)=R2s+s2-(R2s+(xs_d2-delx_d2))*COS(phi_d)
            if(mar_d(j).eq.0) then
             aa1=F1r(Rp,phi_d,R2_d,fy_d,alx_d,xs_d2-delx_d2,
     &               j,nt1,nt11,2,dR2_d)
             aa2=FFR(Rp,phi_d,phi_d1,R2_d,R1_d,fy_d,alx_d,alx_d1,
     &                   xs_d2-delx_d2,dR1_d,dR2_d)
             xyz3(2)=aa1*w_d + aa2*(1.-w_d)
            else
             xyz3(2)=FFR(Rp,phi_d,phi_d1,R2_d,R1_d,fy_d,alx_d,alx_d1,
     &                   xs_d2-delx_d2,dR1_d,dR2_d)
            endif
            xyz3(3)=-(R2s+(xs_d2-delx_d2))*SIN(phi_d)
           ENDIF                          ! for better showing
          else                              ! node3 is in outer zone
           xyz3(1)=R2s+s2
     &            -(R2s-Rd2+R2_d+R2_d*COS(theta_d+delth_d))*COS(phi_d)
           xyz3(2)=R2_d*SIN(theta_d+delth_d)
           xyz3(3)=-(R2s-Rd2+R2_d+R2_d*COS(theta_d+delth_d))*SIN(phi_d)
          endif

          oldx3=tx3; oldy3=ty3; oldz3=tz3         ! previous triangle

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx1=xyz1(1); ty1=xyz1(2); tz1=xyz1(3)   ! for opposite faces
          tx2=xyz2(1); ty2=xyz2(2); tz2=xyz2(3)
          tx3=xyz3(1); ty3=xyz3(2); tz3=xyz3(3)

          oldx2=tx2; oldy2=ty2; oldz2=tz2         ! previous triangle

c-------- second triangle
           xyz1(1)=tx1
           xyz1(2)=ty1
           xyz1(3)=tz1
           xyz2(1)=tx3
           xyz2(2)=ty3
           xyz2(3)=tz3
          if(i.eq.1.and.i.le.n_d) then     ! node3 in the i=1 triangle          
           IF(j.ne.nt1-1+nt11) THEN       ! for better showing
            xyz3(1)=R2s+s2-(R2s+xs_d)*COS(phi_d)
            if(mar_d(j).eq.0) then
             aa1=F1r(Rp,phi_d,R2_d,fy_d,alx_d,xs_d,j,nt1,nt11,2,dR2_d)
             aa2=FFR(Rp,phi_d,phi_d1,R2_d,R1_d,fy_d,alx_d,alx_d1,xs_d,
     &               dR1_d,dR2_d)
             xyz3(2)=aa1*w_d + aa2*(1.-w_d)
            else
             xyz3(2)=FFR(Rp,phi_d,phi_d1,R2_d,R1_d,fy_d,alx_d,alx_d1,
     &                   xs_d,dR1_d,dR2_d)
            endif
            xyz3(3)=-(R2s+xs_d)*SIN(phi_d)
           ELSE
            xyz3(1)=R2s+s2-(R2s+xs_d2)*COS(phi_d)
            if(mar_d(j).eq.0) then
             aa1=F1r(Rp,phi_d,R2_d,fy_d,alx_d,xs_d2,j,nt1,nt11,2,dR2_d)
             aa2=FFR(Rp,phi_d,phi_d1,R2_d,R1_d,fy_d,alx_d,alx_d1,xs_d2,
     &               dR1_d,dR2_d)
             xyz3(2)=aa1*w_d + aa2*(1.-w_d)
            else
             xyz3(2)=FFR(Rp,phi_d,phi_d1,R2_d,R1_d,fy_d,alx_d,alx_d1,
     &                   xs_d2,dR1_d,dR2_d)
            endif
            xyz3(3)=-(R2s+xs_d2)*SIN(phi_d)
           ENDIF                           ! for better showing
          endif
          if(i.eq.1.and.i.gt.n_d) then     
           xyz3(1)=R2s+s2-(R2s-Rd2+R2_d+R2_d*COS(theta_d))*COS(phi_d)
           xyz3(2)=R2_d*SIN(theta_d)
           xyz3(3)=-(R2s-Rd2+R2_d+R2_d*COS(theta_d))*SIN(phi_d)
          endif
          if(i.gt.1) then                     ! use node3 of previous triangle
           xyz3(1)=oldx3
           xyz3(2)=oldy3
           xyz3(3)=oldz3
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx4=xyz3(1); ty4=xyz3(2); tz4=xyz3(3)   ! for opposite faces

c-------- opposite faces
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx3; xyz2(2)=-ty3; xyz2(3)=tz3
           xyz3(1)=tx2; xyz3(2)=-ty2; xyz3(3)=tz2
           call write_facet(xyz1,xyz2,xyz3,errorpass)
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx4; xyz2(2)=-ty4; xyz2(3)=tz4
           xyz3(1)=tx3; xyz3(2)=-ty3; xyz3(3)=tz3
           call write_facet(xyz1,xyz2,xyz3,errorpass)

        enddo              ! angular loop end
      enddo                ! axial loop end



c--------------------------------------------------
c      lower transition (alps < phi < alp)
c--------------------------------------------------

c++++++++++++ left ++++++++++++++++++++++++
      do j=1,nt2                 ! axial loop (left)

        phi_u=alp1s+(alp1-alp1s)*float(j-1)/float(nt2)   ! upper of a face
        phi_d=alp1s+(alp1-alp1s)*float(j)/float(nt2)     ! lower of a face
        R1_u=Rd1; R1_d=Rd1
        
        CALL VAR3(R1s,R2s,Rd1,pi,phi_u,phi_d,xc,zc,rc,xxb,zzb,j,
     &            alx_u,alx_d,s1,s2)

        n_u=num_l; n_d=num_l     ! # of facet at lower transition

        do i=1,na/2              ! angular loop

          theta_u=o90+o90*float(i-n_u-1)/float(na/2-n_u)
          theta_d=o90+o90*float(i-n_d-1)/float(na/2-n_d)
          delth_u=o90/float(na/2-n_u)
          delth_d=o90/float(na/2-n_d)
          xs_u=alx_u*cos((i-1)*o90/n_u) 
          xs_d=alx_d*cos((i-1)*o90/n_d)
          delx_u=alx_u
     &          *(cos((i-1)*o90/n_u)-cos((i)*o90/n_u))
          delx_d=alx_d
     &          *(cos((i-1)*o90/n_d)-cos((i)*o90/n_d)) 

          xs_u2=alx_u-alx_u*float(i-1)/float(n_u)  ! for better showing
          delx_u2=alx_u/float(n_u)
          xs_u2=btr*xs_u+(1-btr)*xs_u2
          delx_u2=btr*delx_u+(1-btr)*delx_u2

c-------- first triangle
          if(i.le.n_u) then                   ! node1 is in inner zone : y(+)
           IF(j.ne.1) THEN                ! for better showing
            xyz1(1)=(R1s+xs_u)*COS(phi_u)-R1s-s1
            xyz1(2)=F2l(R1_u,alx_u,xs_u,R1s,xxb,phi_u,alp1,alp1s,
     &                  j,1,s1)
            xyz1(3)=-(R1s+xs_u)*SIN(phi_u)
           ELSE
            xyz1(1)=(R1s+xs_u2)*COS(phi_u)-R1s-s1
            xyz1(2)=F2l(R1_u,alx_u,xs_u2,R1s,xxb,phi_u,alp1,alp1s,
     &                  j,1,s1)
            xyz1(3)=-(R1s+xs_u2)*SIN(phi_u)
           ENDIF                          ! for better showing
          endif
          if(i.gt.n_u) then                   ! node1 is in outer zone
           xyz1(1)=(R1s-Rd1+R1_u+R1_u*COS(theta_u))*COS(phi_u)-R1s-s1
           xyz1(2)=R1_u*SIN(theta_u)
           xyz1(3)=-(R1s+R1_u*COS(theta_u))*SIN(phi_u)
          endif

          if(i+1.le.n_u) then                 ! node3 is in inner zone : y(+)
           IF(j.ne.1) THEN                ! for better showing
            xyz3(1)=(R1s+(xs_u-delx_u))*COS(phi_u)-R1s-s1
            xyz3(2)=F2l(R1_u,alx_u,xs_u-delx_u,R1s,xxb,
     &                  phi_u,alp1,alp1s,j,1,s1)
            xyz3(3)=-(R1s+(xs_u-delx_u))*SIN(phi_u)
           ELSE
             xyz3(1)=(R1s+(xs_u2-delx_u2))*COS(phi_u)-R1s-s1
            xyz3(2)=F2l(R1_u,alx_u,xs_u2-delx_u2,R1s,xxb,
     &                  phi_u,alp1,alp1s,j,1,s1)
            xyz3(3)=-(R1s+(xs_u2-delx_u2))*SIN(phi_u)
           ENDIF                          ! for better showing
          endif
          if(i+1.gt.n_u) then                 ! node3 is in outer zone
           xyz3(1)=(R1s-Rd1+R1_u+R1_u*COS(theta_u+delth_u))*COS(phi_u)
     &            -R1s-s1
           xyz3(2)=R1_u*SIN(theta_u+delth_u)
           xyz3(3)=-(R1s+R1_u*COS(theta_u+delth_u))*SIN(phi_u)
          endif

          if(i+1.le.n_d) then                 ! node2 is in inner zone : y(+)
           xyz2(1)=(R1s+(xs_d-delx_d))*COS(phi_d)-R1s-s1
           xyz2(2)=F2l(R1_d,alx_d,xs_d-delx_d,R1s,xxb,
     &                 phi_d,alp1,alp1s,j,2,s1)
           xyz2(3)=-(R1s+(xs_d-delx_d))*SIN(phi_d)
          else                                ! node2 is in outer zone
           xyz2(1)=(R1s-Rd1+R1_d+R1_d*COS(theta_d+delth_d))*COS(phi_d)
     &            -R1s-s1
           xyz2(2)=R1_d*SIN(theta_d+delth_d)
           xyz2(3)=-(R1s+R1_d*COS(theta_d+delth_d))*SIN(phi_d)
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx1=xyz1(1); ty1=xyz1(2); tz1=xyz1(3)   ! for opposite faces in case i <= n_u
          tx2=xyz2(1); ty2=xyz2(2); tz2=xyz2(3)
          tx3=xyz3(1); ty3=xyz3(2); tz3=xyz3(3)

c-------- second triangle
           xyz1(1)=tx1
           xyz1(2)=ty1
           xyz1(3)=tz1
           xyz3(1)=tx2
           xyz3(2)=ty2
           xyz3(3)=tz2
          if(i.le.n_d) then                    ! node2 is in inner zone : y(+)
           xyz2(1)=(R1s+xs_d)*COS(phi_d)-R1s-s1
           xyz2(2)=F2l(R1_d,alx_d,xs_d,R1s,xxb,phi_d,alp1,alp1s,
     &                 j,2,s1)
           xyz2(3)=-(R1s+xs_d)*SIN(phi_d)
          else                                 ! node3 is in outer zone
           xyz2(1)=(R1s-Rd1+R1_d+R1_d*COS(theta_d))*COS(phi_d)-R1s-s1
           xyz2(2)=R1_d*SIN(theta_d)
           xyz2(3)=-(R1s+R1_d*COS(theta_d))*SIN(phi_d)
          endif
          if(j.eq.1.and.i.le.n_d) then                 !!!!!!!!!!!!!!!
           if(i.eq.1) then                             !             !
            xp3=xyz2(1);yp3=xyz2(2);zp3=xyz2(3)        !             !
           endif                                       !             !
           po=sqrt((xp3-xyz2(1))**2+(yp3-xyz2(2))**2   !     check   !
     &             +(zp3-xyz2(3))**2)                  !             !
           po=sqrt(po**2-xyz2(2)**2)                   !             !
c          write(11,*) alx_d-po,xyz2(2)                !             !
          endif                                        !!!!!!!!!!!!!!!

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx4=xyz2(1); ty4=xyz2(2); tz4=xyz2(3)   ! for opposite faces

c-------- opposite faces
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx3; xyz2(2)=-ty3; xyz2(3)=tz3
           xyz3(1)=tx2; xyz3(2)=-ty2; xyz3(3)=tz2
           call write_facet(xyz1,xyz2,xyz3,errorpass)
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx2; xyz2(2)=-ty2; xyz2(3)=tz2
           xyz3(1)=tx4; xyz3(2)=-ty4; xyz3(3)=tz4
           call write_facet(xyz1,xyz2,xyz3,errorpass)

        enddo              ! angular loop end
      enddo                ! axial loop end


c++++++++++++ right ++++++++++++++++++++++++
      do j=1,nt2                 ! axial loop (right)

        phi_u=alp2s+(alp2-alp2s)*float(j-1)/float(nt2)   ! upper of a face
        phi_d=alp2s+(alp2-alp2s)*float(j)/float(nt2)     ! lower of a face
        R2_u=Rd2; R2_d=Rd2

        CALL VAR4(R1s,R2s,Rd2,pi,phi_u,phi_d,xc,zc,rc,xxb,zzb,j,
     &            alx_u,alx_d,s1,s2)

        n_u=num_r; n_d=num_r   ! this is only for better showing

        do i=1,na/2              ! angular loop

          theta_u=o90+o90*float(i-n_u-1)/float(na/2-n_u)
          theta_d=o90+o90*float(i-n_d-1)/float(na/2-n_d)
          delth_u=o90/float(na/2-n_u)
          delth_d=o90/float(na/2-n_d)
          xs_u=alx_u*cos((i-1)*o90/n_u)
          xs_d=alx_d*cos((i-1)*o90/n_d)
          delx_u=alx_u
     &          *(cos((i-1)*o90/n_u)-cos((i)*o90/n_u))
          delx_d=alx_d
     &          *(cos((i-1)*o90/n_d)-cos((i)*o90/n_d))

          xs_u2=alx_u-alx_u*float(i-1)/float(n_u)
          delx_u2=alx_u/float(n_u)
          xs_u2=btr*xs_u+(1-btr)*xs_u2
          delx_u2=btr*delx_u+(1-btr)*delx_u2

c-------- first triangle
          if(i.le.n_u) then                 ! node1 is in inner zone : y(+)
           IF(j.ne.1) THEN              ! for better showing
            xyz1(1)=R2s+s2-(R2s+xs_u)*COS(phi_u)
            xyz1(2)=F2r(R2_u,alx_u,xs_u,R2s,xxb,phi_u,alp2,alp2s,
     &                  j,1,s2)
            xyz1(3)=-(R2s+xs_u)*SIN(phi_u)
           ELSE
            xyz1(1)=R2s+s2-(R2s+xs_u2)*COS(phi_u)
            xyz1(2)=F2r(R2_u,alx_u,xs_u2,R2s,xxb,phi_u,alp2,alp2s,
     &                  j,1,s2)
            xyz1(3)=-(R2s+xs_u2)*SIN(phi_u)
           ENDIF                        ! for better showing
          endif
          if(i.gt.n_u) then                 ! node1 is in outer zone
           xyz1(1)=R2s+s2-(R2s-Rd2+R2_u+R2_u*COS(theta_u))*COS(phi_u)
           xyz1(2)=R2_u*SIN(theta_u)
           xyz1(3)=-(R2s-Rd2+R2_u+R2_u*COS(theta_u))*SIN(phi_u)
          endif

          if(i+1.le.n_u) then               ! node2 is in inner zone : y(+)
           IF(j.ne.1) THEN               ! for better showing
            xyz2(1)=R2s+s2-(R2s+xs_u-delx_u)*COS(phi_u)
            xyz2(2)=F2r(R2_u,alx_u,xs_u-delx_u,R2s,xxb,
     &                  phi_u,alp2,alp2s,j,1,s2)
            xyz2(3)=-(R2s+xs_u-delx_u)*SIN(phi_u)
           ELSE
            xyz2(1)=R2s+s2-(R2s+xs_u2-delx_u2)*COS(phi_u)
            xyz2(2)=F2r(R2_u,alx_u,xs_u2-delx_u2,R2s,xxb,
     &                  phi_u,alp2,alp2s,j,1,s2)
            xyz2(3)=-(R2s+xs_u2-delx_u2)*SIN(phi_u)
           ENDIF                         ! for better showing
          endif
          if(i+1.gt.n_u) then               ! node2 is in outer zone
           xyz2(1)=R2s+s2
     &            -(R2s-Rd2+R2_u+R2_u*COS(theta_u+delth_u))*COS(phi_u)
           xyz2(2)=R2_u*SIN(theta_u+delth_u)
           xyz2(3)=-(R2s-Rd2+R2_u+R2_u*COS(theta_u+delth_u))*SIN(phi_u)
          endif

          if(i+1.le.n_d) then               ! node3 is in inner zone : y(+)
           xyz3(1)=R2s+s2-(R2s+xs_d-delx_d)*COS(phi_d)
           xyz3(2)=F2r(R2_d,alx_d,xs_d-delx_d,R2s,xxb,
     &                 phi_d,alp2,alp2s,j,2,s2)
           xyz3(3)=-(R2s+xs_d-delx_d)*SIN(phi_d)
          else                              ! node3 is in outer zone
           xyz3(1)=R2s+s2
     &            -(R2s-Rd2+R2_d+R2_d*COS(theta_d+delth_d))*COS(phi_d)
           xyz3(2)=R2_d*SIN(theta_d+delth_d)
           xyz3(3)=-(R2s-Rd2+R2_d+R2_d*COS(theta_d+delth_d))*SIN(phi_d)
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx1=xyz1(1); ty1=xyz1(2); tz1=xyz1(3)   ! for opposite faces
          tx2=xyz2(1); ty2=xyz2(2); tz2=xyz2(3)
          tx3=xyz3(1); ty3=xyz3(2); tz3=xyz3(3)

c-------- second triangle
           xyz1(1)=tx1
           xyz1(2)=ty1
           xyz1(3)=tz1
           xyz2(1)=tx3
           xyz2(2)=ty3
           xyz2(3)=tz3
          if(i.le.n_d) then                  ! node3 is in inner zone : y(+)
           xyz3(1)=R2s+s2-(R2s+xs_d)*COS(phi_d)
           xyz3(2)=F2r(R2_d,alx_d,xs_d,R2s,xxb,phi_d,alp2,alp2s,
     &                 j,2,s2)
           xyz3(3)=-(R2s+xs_d)*SIN(phi_d)
          else                               ! node3 is in outer zone
           xyz3(1)=R2s+s2-(R2s-Rd2+R2_d+R2_d*COS(theta_d))*COS(phi_d)
           xyz3(2)=R2_d*SIN(theta_d)
           xyz3(3)=-(R2s-Rd2+R2_d+R2_d*COS(theta_d))*SIN(phi_d)
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx4=xyz3(1); ty4=xyz3(2); tz4=xyz3(3)   ! for opposite faces

c-------- opposite faces
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx3; xyz2(2)=-ty3; xyz2(3)=tz3
           xyz3(1)=tx2; xyz3(2)=-ty2; xyz3(3)=tz2
           call write_facet(xyz1,xyz2,xyz3,errorpass)
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx4; xyz2(2)=-ty4; xyz2(3)=tz4
           xyz3(1)=tx3; xyz3(2)=-ty3; xyz3(3)=tz3
           call write_facet(xyz1,xyz2,xyz3,errorpass)

        enddo              ! angular loop end
      enddo                ! axial loop end


c--------------------------------------------------
c      left daughter - curved portion  
c--------------------------------------------------
 
c++++++++++++ left ++++++++++++++++++++++++
      IF(ang1-alp1.gt.0.0) THEN
      do j=1,ndc                 ! axial loop (left)

        phi_u=alp1+(ang1-alp1)*float(j-1)/float(ndc)   ! upper of a face
        phi_d=alp1+(ang1-alp1)*float(j)/float(ndc)     ! lower of a face

        nn=num_l                ! # of faces in inner zone 

        do i=1,na/2              ! angular loop

          theta_o=o90+o90*float(i-nn-1)/float(na/2-nn)  ! outer
          delth_o=o90/float(na/2-nn)
          theta_i=o90*float(i-1)/float(nn)          ! inner
          delth_i=o90/float(nn)

c-------- first triangle
          if(i.le.nn) then                   ! node1 is in inner zone : y(+)
           xyz1(1)=R1s*COS(phi_u)+Rd1*COS(phi_u)*COS(theta_i)-R1s-s1
           xyz1(2)=Rd1*SIN(theta_i)
           xyz1(3)=-R1s*SIN(phi_u)-Rd1*SIN(phi_u)*COS(theta_i)
          endif
          if(i.gt.nn) then                   ! node1 is in outer zone
           xyz1(1)=R1s*COS(phi_u)+Rd1*COS(phi_u)*COS(theta_o)-R1s-s1
           xyz1(2)=Rd1*SIN(theta_o)
           xyz1(3)=-R1s*SIN(phi_u)-Rd1*SIN(phi_u)*COS(theta_o)
          endif

          if(i+1.le.nn) then                 ! node3 is in inner zone : y(+)
           xyz3(1)=R1s*COS(phi_u)
     &            +Rd1*COS(phi_u)*COS(theta_i+delth_i)-R1s-s1
           xyz3(2)=Rd1*SIN(theta_i+delth_i)
           xyz3(3)=-R1s*SIN(phi_u)-Rd1*SIN(phi_u)*COS(theta_i+delth_i)
          endif
          if(i+1.gt.nn) then                 ! node3 is in outer zone
           xyz3(1)=R1s*COS(phi_u)
     &            +Rd1*COS(phi_u)*COS(theta_o+delth_o)-R1s-s1
           xyz3(2)=Rd1*SIN(theta_o+delth_o)
           xyz3(3)=-R1s*SIN(phi_u)-Rd1*SIN(phi_u)*COS(theta_o+delth_o)
          endif

          if(i+1.le.nn) then                 ! node2 is in inner zone : y(+)
           xyz2(1)=R1s*COS(phi_d)
     &            +Rd1*COS(phi_d)*COS(theta_i+delth_i)-R1s-s1
           xyz2(2)=Rd1*SIN(theta_i+delth_i)
           xyz2(3)=-R1s*SIN(phi_d)-Rd1*SIN(phi_d)*COS(theta_i+delth_i)
          else                                ! node2 is in outer zone
           xyz2(1)=R1s*COS(phi_d)
     &            +Rd1*COS(phi_d)*COS(theta_o+delth_o)-R1s-s1
           xyz2(2)=Rd1*SIN(theta_o+delth_o)
           xyz2(3)=-R1s*SIN(phi_d)-Rd1*SIN(phi_d)*COS(theta_o+delth_o)
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx1=xyz1(1); ty1=xyz1(2); tz1=xyz1(3)   ! for opposite faces in case i <= nn
          tx2=xyz2(1); ty2=xyz2(2); tz2=xyz2(3)
          tx3=xyz3(1); ty3=xyz3(2); tz3=xyz3(3)

c-------- second triangle
          xyz1(1)=tx1
          xyz1(2)=ty1
          xyz1(3)=tz1
          xyz3(1)=tx2
          xyz3(2)=ty2
          xyz3(3)=tz2
          if(i.le.nn) then                    ! node2 is in inner zone : y(+)
           xyz2(1)=R1s*COS(phi_d)
     &            +Rd1*COS(phi_d)*COS(theta_i)-R1s-s1
           xyz2(2)=Rd1*SIN(theta_i)
           xyz2(3)=-R1s*SIN(phi_d)-Rd1*SIN(phi_d)*COS(theta_i)
          else                                ! node3 is in outer zone
           xyz2(1)=R1s*COS(phi_d)
     &            +Rd1*COS(phi_d)*COS(theta_o)-R1s-s1
           xyz2(2)=Rd1*SIN(theta_o)
           xyz2(3)=-R1s*SIN(phi_d)-Rd1*SIN(phi_d)*COS(theta_o)
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx4=xyz2(1); ty4=xyz2(2); tz4=xyz2(3)   ! for opposite faces

c-------- opposite faces
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx3; xyz2(2)=-ty3; xyz2(3)=tz3
           xyz3(1)=tx2; xyz3(2)=-ty2; xyz3(3)=tz2
           call write_facet(xyz1,xyz2,xyz3,errorpass)
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx2; xyz2(2)=-ty2; xyz2(3)=tz2
           xyz3(1)=tx4; xyz3(2)=-ty4; xyz3(3)=tz4
           call write_facet(xyz1,xyz2,xyz3,errorpass)

        enddo              ! angular loop end
      enddo                ! axial loop end
      ENDIF


c++++++++++++ right ++++++++++++++++++++++++
      IF(ang2-alp2.gt.0.0) THEN
      do j=1,ndc                 ! axial loop (right)

        phi_u=alp2+(ang2-alp2)*float(j-1)/float(ndc)   ! upper of a face
        phi_d=alp2+(ang2-alp2)*float(j)/float(ndc)     ! lower of a face

        nn=num_r                ! # of faces in inner zone

        do i=1,na/2              ! angular loop

          theta_o=o90+o90*float(i-nn-1)/float(na/2-nn)  ! outer
          delth_o=o90/float(na/2-nn)
          theta_i=o90*float(i-1)/float(nn)          ! inner
          delth_i=o90/float(nn)

c-------- first triangle
          if(i.le.nn) then                 ! node1 is in inner zone : y(+)
           xyz1(1)=R2s+s2-R2s*COS(phi_u)-Rd2*COS(phi_u)*COS(theta_i)
           xyz1(2)=Rd2*SIN(theta_i)
           xyz1(3)=-R2s*SIN(phi_u)-R2_u*SIN(phi_u)*COS(theta_i)
          endif
          if(i.gt.nn) then                 ! node1 is in outer zone
           xyz1(1)=R2s+s2-R2s*COS(phi_u)-Rd2*COS(phi_u)*COS(theta_o)
           xyz1(2)=Rd2*SIN(theta_o)
           xyz1(3)=-R2s*SIN(phi_u)-R2_u*SIN(phi_u)*COS(theta_o)
          endif

          if(i+1.le.nn) then               ! node2 is in inner zone : y(+)
           xyz2(1)=R2s+s2-R2s*COS(phi_u)
     &            -Rd2*COS(phi_u)*COS(theta_i+delth_i)
           xyz2(2)=Rd2*SIN(theta_i+delth_i)
           xyz2(3)=-R2s*SIN(phi_u)-Rd2*SIN(phi_u)*COS(theta_i+delth_i)
          endif
          if(i+1.gt.nn) then               ! node2 is in outer zone
           xyz2(1)=R2s+s2-R2s*COS(phi_u)
     &            -Rd2*COS(phi_u)*COS(theta_o+delth_o)
           xyz2(2)=Rd2*SIN(theta_o+delth_o)
           xyz2(3)=-R2s*SIN(phi_u)-Rd2*SIN(phi_u)*COS(theta_o+delth_o)
          endif

          if(i+1.le.nn) then               ! node3 is in inner zone : y(+)
           xyz3(1)=R2s+s2-R2s*COS(phi_d)
     &            -Rd2*COS(phi_d)*COS(theta_i+delth_i)
           xyz3(2)=Rd2*SIN(theta_i+delth_i)
           xyz3(3)=-R2s*SIN(phi_d)-Rd2*SIN(phi_d)*COS(theta_i+delth_i)
          else                              ! node3 is in outer zone
           xyz3(1)=R2s+s2-R2s*COS(phi_d)
     &            -Rd2*COS(phi_d)*COS(theta_o+delth_o)
           xyz3(2)=Rd2*SIN(theta_o+delth_o)
           xyz3(3)=-R2s*SIN(phi_d)-Rd2*SIN(phi_d)*COS(theta_o+delth_o)
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx1=xyz1(1); ty1=xyz1(2); tz1=xyz1(3)   ! for opposite faces
          tx2=xyz2(1); ty2=xyz2(2); tz2=xyz2(3)
          tx3=xyz3(1); ty3=xyz3(2); tz3=xyz3(3)

c-------- second triangle
           xyz1(1)=tx1
           xyz1(2)=ty1
           xyz1(3)=tz1
           xyz2(1)=tx3
           xyz2(2)=ty3
           xyz2(3)=tz3
          if(i.le.nn) then                  ! node3 is in inner zone : y(+)
           xyz3(1)=R2s+s2-R2s*COS(phi_d)-Rd2*COS(phi_d)*COS(theta_i)
           xyz3(2)=Rd2*SIN(theta_i)
           xyz3(3)=-R2s*SIN(phi_d)-Rd2*SIN(phi_d)*COS(theta_i)
          else                               ! node3 is in outer zone
           xyz3(1)=R2s+s2-R2s*COS(phi_d)-Rd2*COS(phi_d)*COS(theta_o)
           xyz3(2)=Rd2*SIN(theta_o)
           xyz3(3)=-R2s*SIN(phi_d)-Rd2*SIN(phi_d)*COS(theta_o)
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx4=xyz3(1); ty4=xyz3(2); tz4=xyz3(3)   ! for opposite faces

c-------- opposite faces
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx3; xyz2(2)=-ty3; xyz2(3)=tz3
           xyz3(1)=tx2; xyz3(2)=-ty2; xyz3(3)=tz2
           call write_facet(xyz1,xyz2,xyz3,errorpass)
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx4; xyz2(2)=-ty4; xyz2(3)=tz4
           xyz3(1)=tx3; xyz3(2)=-ty3; xyz3(3)=tz3
           call write_facet(xyz1,xyz2,xyz3,errorpass)

        enddo              ! angular loop end
      enddo                ! axial loop end
      ENDIF


c--------------------------------------------------
c      left daughter - straight portion
c--------------------------------------------------

c++++++++++++ left ++++++++++++++++++++++++
      IF(bld1st.gt.0.0) THEN
      do j=1,nd                 ! axial loop (left)

        x1_u=R1s*cos(ang1)-R1s-s1-bld1st*sin(ang1)*float(j-1)/float(nd)
        x1_d=R1s*cos(ang1)-R1s-s1-bld1st*sin(ang1)*float(j)/float(nd)
        z1_u=-R1s*sin(ang1)-bld1st*cos(ang1)*float(j-1)/float(nd)
        z1_d=-R1s*sin(ang1)-bld1st*cos(ang1)*float(j)/float(nd)

        nn=num_l                ! # of faces in inner zone

        do i=1,na/2              ! angular loop

          theta_o=o90+o90*float(i-nn-1)/float(na/2-nn)  ! outer
          delth_o=o90/float(na/2-nn)
          theta_i=o90*float(i-1)/float(nn)              ! inner
          delth_i=o90/float(nn)

c-------- first triangle
          if(i.le.nn) then                   ! node1 is in inner zone : y(+)
           xyz1(1)=x1_u+Rd1*COS(ang1)*COS(theta_i)
           xyz1(2)=Rd1*SIN(theta_i)
           xyz1(3)=z1_u-Rd1*SIN(ang1)*COS(theta_i)
          endif
          if(i.gt.nn) then                   ! node1 is in outer zone
           xyz1(1)=x1_u+Rd1*COS(ang1)*COS(theta_o)
           xyz1(2)=Rd1*SIN(theta_o)
           xyz1(3)=z1_u-Rd1*SIN(ang1)*COS(theta_o)
          endif

          if(i+1.le.nn) then                 ! node3 is in inner zone : y(+)
           xyz3(1)=x1_u
     &            +Rd1*COS(ang1)*COS(theta_i+delth_i)
           xyz3(2)=Rd1*SIN(theta_i+delth_i)
           xyz3(3)=z1_u-Rd1*SIN(ang1)*COS(theta_i+delth_i)
          endif
          if(i+1.gt.nn) then                 ! node3 is in outer zone
           xyz3(1)=x1_u
     &            +Rd1*COS(ang1)*COS(theta_o+delth_o)
           xyz3(2)=Rd1*SIN(theta_o+delth_o)
           xyz3(3)=z1_u-Rd1*SIN(ang1)*COS(theta_o+delth_o)
          endif

          if(i+1.le.nn) then                 ! node2 is in inner zone : y(+)
           xyz2(1)=x1_d
     &            +Rd1*COS(ang1)*COS(theta_i+delth_i)
           xyz2(2)=Rd1*SIN(theta_i+delth_i)
           xyz2(3)=z1_d-Rd1*SIN(ang1)*COS(theta_i+delth_i)
          else                                ! node2 is in outer zone
           xyz2(1)=x1_d
     &            +Rd1*COS(ang1)*COS(theta_o+delth_o)
           xyz2(2)=Rd1*SIN(theta_o+delth_o)
           xyz2(3)=z1_d-Rd1*SIN(ang1)*COS(theta_o+delth_o)
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx1=xyz1(1); ty1=xyz1(2); tz1=xyz1(3)   ! for opposite faces in case i <= nn
          tx2=xyz2(1); ty2=xyz2(2); tz2=xyz2(3)
          tx3=xyz3(1); ty3=xyz3(2); tz3=xyz3(3)

c-------- second triangle
          xyz1(1)=tx1
          xyz1(2)=ty1
          xyz1(3)=tz1
          xyz3(1)=tx2
          xyz3(2)=ty2
          xyz3(3)=tz2
          if(i.le.nn) then                    ! node2 is in inner zone : y(+)
           xyz2(1)=x1_d
     &            +Rd1*COS(ang1)*COS(theta_i)
           xyz2(2)=Rd1*SIN(theta_i)
           xyz2(3)=z1_d-Rd1*SIN(ang1)*COS(theta_i)
          else                                ! node3 is in outer zone
           xyz2(1)=x1_d
     &            +Rd1*COS(ang1)*COS(theta_o)
           xyz2(2)=Rd1*SIN(theta_o)
           xyz2(3)=z1_d-Rd1*SIN(ang1)*COS(theta_o)
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx4=xyz2(1); ty4=xyz2(2); tz4=xyz2(3)   ! for opposite faces

c-------- opposite faces
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx3; xyz2(2)=-ty3; xyz2(3)=tz3
           xyz3(1)=tx2; xyz3(2)=-ty2; xyz3(3)=tz2
           call write_facet(xyz1,xyz2,xyz3,errorpass)
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx2; xyz2(2)=-ty2; xyz2(3)=tz2
           xyz3(1)=tx4; xyz3(2)=-ty4; xyz3(3)=tz4
           call write_facet(xyz1,xyz2,xyz3,errorpass)

        enddo              ! angular loop end
      enddo                ! axial loop end
      ENDIF


c++++++++++++ right ++++++++++++++++++++++++
      IF(bld2st.gt.0.0) THEN
      do j=1,nd                 ! axial loop (right)

        x2_u=R2s+s2-R2s*cos(ang2)+bld2st*sin(ang2)*float(j-1)/float(nd)
        x2_d=R2s+s2-R2s*cos(ang2)+bld2st*sin(ang2)*float(j)/float(nd)
        z2_u=-R2s*sin(ang2)-bld2st*cos(ang2)*float(j-1)/float(nd)
        z2_d=-R2s*sin(ang2)-bld2st*cos(ang2)*float(j)/float(nd)

        nn=num_r                ! # of faces in inner zone

        do i=1,na/2              ! angular loop

          theta_o=o90+o90*float(i-nn-1)/float(na/2-nn)  ! outer
          delth_o=o90/float(na/2-nn)
          theta_i=o90*float(i-1)/float(nn)          ! inner
          delth_i=o90/float(nn)

c-------- first triangle
          if(i.le.nn) then                 ! node1 is in inner zone : y(+)
           xyz1(1)=x2_u-Rd2*COS(ang2)*COS(theta_i)
           xyz1(2)=Rd2*SIN(theta_i)
           xyz1(3)=z2_u-R2_u*SIN(ang2)*COS(theta_i)
          endif
          if(i.gt.nn) then                 ! node1 is in outer zone
           xyz1(1)=x2_u-Rd2*COS(ang2)*COS(theta_o)
           xyz1(2)=Rd2*SIN(theta_o)
           xyz1(3)=z2_u-R2_u*SIN(ang2)*COS(theta_o)
          endif

          if(i+1.le.nn) then               ! node2 is in inner zone : y(+)
           xyz2(1)=x2_u
     &            -Rd2*COS(ang2)*COS(theta_i+delth_i)
           xyz2(2)=Rd2*SIN(theta_i+delth_i)
           xyz2(3)=z2_u-Rd2*SIN(ang2)*COS(theta_i+delth_i)
          endif
          if(i+1.gt.nn) then               ! node2 is in outer zone
           xyz2(1)=x2_u
     &            -Rd2*COS(ang2)*COS(theta_o+delth_o)
           xyz2(2)=Rd2*SIN(theta_o+delth_o)
           xyz2(3)=z2_u-Rd2*SIN(ang2)*COS(theta_o+delth_o)
          endif

          if(i+1.le.nn) then               ! node3 is in inner zone : y(+)
           xyz3(1)=x2_d
     &            -Rd2*COS(ang2)*COS(theta_i+delth_i)
           xyz3(2)=Rd2*SIN(theta_i+delth_i)
           xyz3(3)=z2_d-Rd2*SIN(ang2)*COS(theta_i+delth_i)
          else                              ! node3 is in outer zone
           xyz3(1)=x2_d
     &            -Rd2*COS(ang2)*COS(theta_o+delth_o)
           xyz3(2)=Rd2*SIN(theta_o+delth_o)
           xyz3(3)=z2_d-Rd2*SIN(ang2)*COS(theta_o+delth_o)
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx1=xyz1(1); ty1=xyz1(2); tz1=xyz1(3)   ! for opposite faces
          tx2=xyz2(1); ty2=xyz2(2); tz2=xyz2(3)
          tx3=xyz3(1); ty3=xyz3(2); tz3=xyz3(3)

c-------- second triangle
           xyz1(1)=tx1
           xyz1(2)=ty1
           xyz1(3)=tz1
           xyz2(1)=tx3
           xyz2(2)=ty3
           xyz2(3)=tz3
          if(i.le.nn) then                  ! node3 is in inner zone : y(+)
           xyz3(1)=x2_d-Rd2*COS(ang2)*COS(theta_i)
           xyz3(2)=Rd2*SIN(theta_i)
           xyz3(3)=z2_d-Rd2*SIN(ang2)*COS(theta_i)
          else                               ! node3 is in outer zone
           xyz3(1)=x2_d-Rd2*COS(ang2)*COS(theta_o)
           xyz3(2)=Rd2*SIN(theta_o)
           xyz3(3)=z2_d-Rd2*SIN(ang2)*COS(theta_o)
          endif

          call write_facet(xyz1,xyz2,xyz3,errorpass)

          tx4=xyz3(1); ty4=xyz3(2); tz4=xyz3(3)   ! for opposite faces

c-------- opposite faces
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx3; xyz2(2)=-ty3; xyz2(3)=tz3
           xyz3(1)=tx2; xyz3(2)=-ty2; xyz3(3)=tz2
           call write_facet(xyz1,xyz2,xyz3,errorpass)
           xyz1(1)=tx1; xyz1(2)=-ty1; xyz1(3)=tz1
           xyz2(1)=tx4; xyz2(2)=-ty4; xyz2(3)=tz4
           xyz3(1)=tx3; xyz3(2)=-ty3; xyz3(3)=tz3
           call write_facet(xyz1,xyz2,xyz3,errorpass)

        enddo              ! angular loop end
      enddo                ! axial loop end
      ENDIF


      return
      end


c******************** subroutine VAR1 **********************************
c this fuction returns some variables for upper transition zone (left)
c the returned variables are alx, fy, n
c alx : distance btw center line of a branch & dividing line
c n   : half number of faces in the inner zone
c R_* : radius of torus
c***********************************************************************

      SUBROUTINE VAR1(R1s,R2s,Rp,Rd1,Rd2,na,pi,alp1s,phi_u,phi_d,phi1s,
     &phi2s,alx_u,alx_d,n_u,n_d,R1_u,R1_d,zzb,xxb,nt1,nt11,j,dv,s1,s2)


      if(phi_u.eq.0.0) then   ! if face is upper most one

        fy_u=Rp; R1_u=Rp
        alx_u=(s1+s2)/2.

        tmp1=float(na/2)*(alx_u/(alx_u+Rd1))
        tmp2=INT(tmp1)
        if((tmp1-tmp2).ge.0.5) then              ! number of face at inner transition zone
         n_u=tmp2+1              
        else
         n_u=tmp2
        endif
        if(n_u.eq.0) n_u=1

        si_d=-2.*(phi_d/phi1s)**3+3.*(phi_d/phi1s)**2         ! return radius of a branch
        if(phi_d/phi1s.ge.1.0) si_d=1.
        R1_d=Rp-(Rp-Rd1)*si_d

        c1d=phi_d
        ttd=-2.*(c1d/alp1s)**3+3.*(c1d/alp1s)**2   ! 0-1
        c2d=ttd**dv*c1d*phi2s/phi1s
     &    +(1.-ttd**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c1d))
        x_d=(-tan(c1d)*(R1s+s1)+tan(c2d)*(R2s+s2))/(tan(c1d)+tan(c2d))     ! point on dividing line
        z_d=-tan(c1d)*tan(c2d)*(R1s+R2s+s1+s2)/(tan(c1d)+tan(c2d))
        xc_d=R1s*cos(c1d)-R1s-s1                               ! point on center line of a branch
        zc_d=-R1s*sin(c1d)

        alx_d=SQRT((xc_d-x_d)**2+(zc_d-z_d)**2)  ! distance btw center of a branch & dividing line

        tmp1=float(na/2)*(alx_d/(alx_d+Rd1))
        tmp2=INT(tmp1)
        if((tmp1-tmp2).ge.0.5) then              ! number of face at inner transition zone
         n_d=tmp2+1                
        else
         n_d=tmp2
        endif
        if(n_d.eq.0) n_d=1

      else

        si_u=-2.*(phi_u/phi1s)**3+3.*(phi_u/phi1s)**2         ! return radius of a branch
        if(phi_u/phi1s.ge.1.0) si_u=1.
        R1_u=Rp-(Rp-Rd1)*si_u
        si_d=-2.*(phi_d/phi1s)**3+3.*(phi_d/phi1s)**2  
        if(phi_d/phi1s.ge.1.0) si_d=1.
        R1_d=Rp-(Rp-Rd1)*si_d


        c1u=phi_u
        ttu=-2.*(c1u/alp1s)**3+3.*(c1u/alp1s)**2   ! 0-1
        c2u=ttu**dv*c1u*phi2s/phi1s
     &    +(1.-ttu**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c1u))
        c1d=phi_d
        ttd=-2.*(c1d/alp1s)**3+3.*(c1d/alp1s)**2   ! 0-1
        c2d=ttd**dv*c1d*phi2s/phi1s
     &    +(1.-ttd**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c1d))
        x_u=(-tan(c1u)*(R1s+s1)+tan(c2u)*(R2s+s2))/(tan(c1u)+tan(c2u))     ! point on dividing line
        z_u=-tan(c1u)*tan(c2u)*(R1s+R2s+s1+s2)/(tan(c1u)+tan(c2u))
        x_d=(-tan(c1d)*(R1s+s1)+tan(c2d)*(R2s+s2))/(tan(c1d)+tan(c2d))
        z_d=-tan(c1d)*tan(c2d)*(R1s+R2s+s1+s2)/(tan(c1d)+tan(c2d))
        xc_u=R1s*cos(c1u)-R1s-s1
        zc_u=-R1s*sin(c1u)
        xc_d=R1s*cos(c1d)-R1s-s1
        zc_d=-R1s*sin(c1d)

        alx_u=SQRT((xc_u-x_u)**2+(zc_u-z_u)**2)  ! distance btw center of a branch & dividing line
        alx_d=SQRT((xc_d-x_d)**2+(zc_d-z_d)**2)
        if(j.eq.nt1-1+nt11) alx_d=SQRT((xc_d-xxb)**2+(zc_d-zzb)**2)

        tmp1=float(na/2)*(alx_u/(alx_u+Rd1))
        tmp2=INT(tmp1)
        if((tmp1-tmp2).ge.0.5) then              ! number of face at inner transition zone
         n_u=tmp2+1                  
        else
         n_u=tmp2
        endif
        if(n_u.eq.0) n_u=1
        tmp1=float(na/2)*(alx_d/(alx_d+Rd1))
        tmp2=INT(tmp1)
        if((tmp1-tmp2).ge.0.5) then     
         n_d=tmp2+1                  
        else
         n_d=tmp2
        endif
        if(n_d.eq.0) n_d=1

      endif

      return
      end


c******************** subroutine VAR2 **********************************
c this fuction returns some variables for upper transition zone (right)
c the returned variables are alx, fy, n
c alx : distance btw center line of a branch & dividing line
c n   :  half number of faces in the inner zone
c R_* : radius of torus
c***********************************************************************

      SUBROUTINE VAR2(R1s,R2s,Rp,Rd1,Rd2,na,pi,alp1s,alp2s,
     &phi_u,phi_d,phi1s,
     &phi2s,alx_u,alx_d,n_u,n_d,R2_u,R2_d,zzb,xxb,nt1,nt11,j,dv,s1,s2)


      if(phi_u.eq.0.0) then   ! if face is upper most one

        fy_u=Rp; R2_u=Rp
        alx_u=(s1+s2)/2.

        tmp1=float(na/2)*(alx_u/(alx_u+Rd2))
        tmp2=INT(tmp1)
        if((tmp1-tmp2).ge.0.5) then              ! number of face at inner transition zone
         n_u=tmp2+1              
        else
         n_u=tmp2
        endif
        if(n_u.eq.0) n_u=1

        si_d=-2.*(phi_d/phi2s)**3+3.*(phi_d/phi2s)**2         ! return radius of a branch
        if(phi_d/phi2s.ge.1.0) si_d=1.0
        R2_d=Rp-(Rp-Rd2)*si_d

        c2d=phi_d
        ttd=-2.*(c2d/alp2s)**3+3.*(c2d/alp2s)**2   ! 0-1
        c1d=ttd**dv*c2d*phi1s/phi2s
     &    +(1.-ttd**dv)*atan(
     &   (R2s+s2-(-s1+s2)/2.)/(R1s+s1+(-s1+s2)/2.)*tan(c2d))
        x_d=(-tan(c1d)*(R1s+s1)+tan(c2d)*(R2s+s2))/(tan(c1d)+tan(c2d))     ! point on dividing line
        z_d=-tan(c1d)*tan(c2d)*(R1s+R2s+s1+s2)/(tan(c1d)+tan(c2d))
        xc_d=R2s+s2-R2s*cos(c2d)                               ! point on center line of a branch
        zc_d=-R2s*sin(c2d)

        alx_d=SQRT((xc_d-x_d)**2+(zc_d-z_d)**2)  ! distance btw center of a branch & dividing line

        tmp1=float(na/2)*(alx_d/(alx_d+Rd2))
        tmp2=INT(tmp1)
        if((tmp1-tmp2).ge.0.5) then              ! number of face at inner transition zone
         n_d=tmp2+1
        else
         n_d=tmp2
        endif
        if(n_d.eq.0) n_d=1

      else

        si_u=-2.*(phi_u/phi2s)**3+3.*(phi_u/phi2s)**2         ! return radius of a branch
        if(phi_u/phi2s.ge.1.0) si_u=1.0
        R2_u=Rp-(Rp-Rd2)*si_u
        si_d=-2.*(phi_d/phi2s)**3+3.*(phi_d/phi2s)**2
        if(phi_d/phi2s.ge.1.0) si_d=1.0
        R2_d=Rp-(Rp-Rd2)*si_d

        c2u=phi_u
        ttu=-2.*(c2u/alp2s)**3+3.*(c2u/alp2s)**2   ! 0-1
        c1u=ttu**dv*c2u*phi1s/phi2s
     &    +(1.-ttu**dv)*atan(
     &   (R2s+s2-(-s1+s2)/2.)/(R1s+s1+(-s1+s2)/2.)*tan(c2u))
        c2d=phi_d
        ttd=-2.*(c2d/alp2s)**3+3.*(c2d/alp2s)**2   ! 0-1
        c1d=ttd**dv*c2d*phi1s/phi2s
     &    +(1.-ttd**dv)*atan(
     &   (R2s+s2-(-s1+s2)/2.)/(R1s+s1+(-s1+s2)/2.)*tan(c2d))
        x_u=(-tan(c1u)*(R1s+s1)+tan(c2u)*(R2s+s2))/(tan(c1u)+tan(c2u))     ! point on dividing line
        z_u=-tan(c1u)*tan(c2u)*(R1s+R2s+s1+s2)/(tan(c1u)+tan(c2u))
        x_d=(-tan(c1d)*(R1s+s1)+tan(c2d)*(R2s+s2))/(tan(c1d)+tan(c2d))
        z_d=-tan(c1d)*tan(c2d)*(R1s+R2s+s1+s2)/(tan(c1d)+tan(c2d))
        xc_u=R2s+s2-R2s*cos(c2u)
        zc_u=-R2s*sin(c2u)
        xc_d=R2s+s2-R2s*cos(c2d)
        zc_d=-R2s*sin(c2d)
        
c        if(x_u.ge.xc_u) print *,'dividing line crossing',j

        alx_u=SQRT((xc_u-x_u)**2+(zc_u-z_u)**2)  ! distance btw center of a branch & dividing line
        alx_d=SQRT((xc_d-x_d)**2+(zc_d-z_d)**2)
        if(j.eq.nt1-1+nt11) alx_d=SQRT((xc_d-xxb)**2+(zc_d-zzb)**2)

        tmp1=float(na/2)*(alx_u/(alx_u+Rd2))
        tmp2=INT(tmp1)
        if((tmp1-tmp2).ge.0.5) then              ! number of face at inner transition zone
         n_u=tmp2+1
        else
         n_u=tmp2
        endif
        if(n_u.eq.0) n_u=1
        tmp1=float(na/2)*(alx_d/(alx_d+Rd2))
        tmp2=INT(tmp1)
        if((tmp1-tmp2).ge.0.5) then
         n_d=tmp2+1
        else
         n_d=tmp2
        endif
        if(n_d.eq.0) n_d=1

      endif

      return
      end


c******************** subroutine VAR3 **********************************
c this fuction returns some variables for lower transition zone (left)
c the returned variables are alx, n
c alx : distance btw center line of a branch & carinar circle
c n   :  half number of faces in the inner zone
c***********************************************************************

      SUBROUTINE VAR3(R1s,R2s,Rd1,pi,phi_u,phi_d,xc,zc,rc,xxb,zzb,j,
     &                alx_u,alx_d,s1,s2)

       R1_u=Rd1; R1_d=Rd1

       a_u=1.+(tan(phi_u))**2
       b_u=tan(phi_u)*zc+(R1s+s1)*(tan(phi_u))**2-xc 
       c_u=xc**2-rc**2+zc**2+2.*tan(phi_u)*zc*(R1s+s1)
     &    +(tan(phi_u)*(R1s+s1))**2
       a_d=1.+(tan(phi_d))**2
       b_d=tan(phi_d)*zc+(R1s+s1)*(tan(phi_d))**2-xc
       c_d=xc**2-rc**2+zc**2+2.*tan(phi_d)*zc*(R1s+s1)
     &    +(tan(phi_d)*(R1s+s1))**2

       x_u=(-b_u-SQRT(b_u**2-a_u*c_u))/a_u     ! point on carinar circle
       z_u=-tan(phi_u)*(x_u+R1s+s1)
       x_d=(-b_d-SQRT(b_d**2-a_d*c_d))/a_d
       z_d=-tan(phi_d)*(x_d+R1s+s1)
       xc_u=R1s*cos(phi_u)-R1s-s1
       zc_u=-R1s*sin(phi_u)
       xc_d=R1s*cos(phi_d)-R1s-s1
       zc_d=-R1s*sin(phi_d)

       alx_u=SQRT((xc_u-x_u)**2+(zc_u-z_u)**2)  ! distance btw center of a branch & carinar circle
       if(j.eq.1) alx_u=SQRT((xc_u-xxb)**2+(zc_u-zzb)**2)
       alx_d=SQRT((xc_d-x_d)**2+(zc_d-z_d)**2)

      return
      end


c******************** subroutine VAR4 **********************************
c this fuction returns some variables for lower transition zone (right)
c the returned variables are alx, n
c alx : distance btw center line of a branch & carinar circle
c n   :  half number of faces in the inner zone
c***********************************************************************

      SUBROUTINE VAR4(R1s,R2s,Rd2,pi,phi_u,phi_d,xc,zc,rc,xxb,zzb,j,
     &                alx_u,alx_d,s1,s2)

       R2_u=Rd2; R2_d=Rd2

       a_u=1.+(tan(phi_u))**2
       b_u=-(tan(phi_u)*zc+(R2s+s2)*(tan(phi_u))**2+xc)
       c_u=xc**2-rc**2+zc**2+2.*tan(phi_u)*zc*(R2s+s2)
     &    +(tan(phi_u)*(R2s+s2))**2
       a_d=1.+(tan(phi_d))**2
       b_d=-(tan(phi_d)*zc+(R2s+s2)*(tan(phi_d))**2+xc)
       c_d=xc**2-rc**2+zc**2+2.*tan(phi_d)*zc*(R2s+s2)
     &    +(tan(phi_d)*(R2s+s2))**2

       x_u=(-b_u+SQRT(b_u**2-a_u*c_u))/a_u     ! point on carinar circle
       z_u=tan(phi_u)*(x_u-R2s-s2)
       x_d=(-b_d+SQRT(b_d**2-a_d*c_d))/a_d
       z_d=tan(phi_d)*(x_d-R2s-s2)
       xc_u=R2s+s2-R2s*cos(phi_u)
       zc_u=-R2s*sin(phi_u)
       xc_d=R2s+s2-R2s*cos(phi_d)
       zc_d=-R2s*sin(phi_d)

       alx_u=SQRT((xc_u-x_u)**2+(zc_u-z_u)**2)  ! distance btw center of a branch & carinar circle
       if(j.eq.1) alx_u=SQRT((xc_u-xxb)**2+(zc_u-zzb)**2)
       alx_d=SQRT((xc_d-x_d)**2+(zc_d-z_d)**2)

       return
       end


c******************** subroutine writefacet ****************************
c this function accepts as input three 3-d coordinates describing the
c vertices of a triangular facet on the solid object.
c
c the order of the triples must satisfy the right hand rule, which is
c that the normal points out of the face defined by the order of these
c points using the right hand order rule
c***********************************************************************
      SUBROUTINE write_facet(xyz1,xyz2,xyz3,errorpass)

      DIMENSION xyz1(3),xyz2(3),xyz3(3)
      logical errorpass

c write the facet info

      if(errorpass) return

      write(66,*)   ' Shape'
      write(66,*)   ' {'
      write(66,*)   '  appearance Appearance {'
      write(66,*)   '   material Material {'
      write(66,*)   '    diffuseColor 0.7 0.4 0.2'
      write(66,*)   '    emissiveColor 0.05 0.05 0.05'
      write(66,*)   '   }'
      write(66,*)   '  }'
      write(66,*)   '  geometry IndexedFaceSet {'
      write(66,*)   '   coord Coordinate {'
      write(66,*)   '    point ['
      write(66,100) '        ',xyz1(1),xyz1(2),xyz1(3),','
      write(66,100) '        ',xyz2(1),xyz2(2),xyz2(3),','
      write(66,100) '        ',xyz3(1),xyz3(2),xyz3(3),','
      write(66,*)   '    ]'
      write(66,*)   '   }'
      write(66,*)   '   coordIndex [0 1 2 -1]'
      write(66,*)   '   solid FALSE'
      write(66,*)   '  }'
      write(66,*)   ' }'
  100 format(a,3(e12.5,2x),a)

      return
      end


c**************** subroutine header **************************
c     this routine writes the VRML file header
c*************************************************************
      SUBROUTINE header(title,errorpass)

      character*(*) title
      logical errorpass

      if(errorpass) return

      write(66,'(a)') '#VRML V2.0 utf8'
      write(66,*) 'WorldInfo'
      write(66,*) '{'
      write(66,*) 'info ["Wexler group lung VRML"]'
      write(66,*) 'title "' ,title, '"'
      write(66,*) '}'

      return
      end


c***************** subroutine gaussj *************************
c    this subroutine returns matrix-inverse and  
c    solution vector 
c*************************************************************
      SUBROUTINE gaussj(a,n,np,b,m,mp)
      INTEGER m,n,NMAX
      REAL a(np,np),b(np,mp)
      PARAMETER (NMAX=50)
      INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),ipiv(NMAX)
      REAL big,dum,pivinv
      do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (abs(a(j,k)).ge.big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
                pause 'singular matrix in gaussj'
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol).eq.0.) pause 'singular matrix in gaussj'
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue

      return
      END


c******************** subroutine HEIGHT *****************************
c this fuction returns the height at dividing line
c******************************************************************

      SUBROUTINE HEIGHT(R1s,R2s,Rp,Rd1,Rd2,alp1s,alp2s,phi1s,phi2s,
     &           j,nt1,nt11,ff_u,ff_d,zzb,mar_u,mar_d,
     &           bt1l,bt1r,bt2l,bt2r,dv,s1,s2)

      DIMENSION ff_u(100),ff_d(100)
      DIMENSION mar_u(100),mar_d(100)


      !---- for interpolation btw single-poly region and double-poly region
      ! bt1 is the angle where single-poly ends
      ! bt2 is the angle where interpolation ends

      bt1l=0.0; bt1r=0.0; bt2l=0.0; bt2r=0.0
      rat=2.0  ! interpolation region (bt2=rat*bt1)

      xx=0.05

      do j=1,5000

       c1=alp1s*float(j)/float(5000)          
       tt=-2.*(c1/alp1s)**3+3.*(c1/alp1s)**2   ! 0-1 
       c2=tt**dv*c1*phi2s/phi1s                ! dividing line
     &  +(1.-tt**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c1))  

       w1=-2.*(c1/phi1s)**3+3.*(c1/phi1s)**2
       w2=-2.*(c2/phi2s)**3+3.*(c2/phi2s)**2
       if(c1/phi1s.ge.1.0) w1=1.0
       if(c2/phi2s.ge.1.0) w2=1.0
       R1=Rp-(Rp-Rd1)*w1
       h=HEIG(c1,alp1s,Rp,R1)
       R2=Rp-(Rp-Rd2)*w2

       delR1=R1-Rd1
       delR2=R2-Rd2

       x=(-tan(c1)*(R1s+s1)+tan(c2)*(R2s+s2))/(tan(c1)+tan(c2))  ! dividing line coordinates
       z=-tan(c1)*tan(c2)*(R1s+R2s+s1+s2)/(tan(c1)+tan(c2))

       xc1=R1s*cos(c1)-R1s-s1
       zc1=-R1s*sin(c1)
       alx1=SQRT((xc1-x)**2+(zc1-z)**2)
       ratio=(Rp-h)/Rp
       alt1=(0.6*alx1+0.2*alx1*ratio)+xx*alx1    ! xx1   model2
       R1tmp=sqrt(R1**2-(alt1-delR1)**2)

       xc2=R2s+s2-R2s*cos(c2)                              
       zc2=-R2s*sin(c2)
       alx2=SQRT((xc2-x)**2+(zc2-z)**2)
       ratio=(Rp-h)/Rp
       alt2=(0.6*alx2+0.2*alx2*ratio)+xx*alx2    ! xx2   model2
       R2tmp=sqrt(R2**2-(alt2-delR2)**2)
       
       if(h.lt.R2tmp.and.h.lt.R1tmp) then 
         bt1l=c1; bt1r=c2
         bt2l=rat*bt1l
         bt2r=rat*bt1r
         if(bt2l.ge.alp1s) bt2l=alp1s
         if(bt2r.ge.alp2s) bt2r=alp2s
         goto 111
       endif

      enddo
      !---- for interpolation btw single-poly region and double-poly region
     

111   do j=1,nt1-1+nt11               ! left
       mar_u(j)=0; mar_d(j)=0

       if(j.le.nt1-1) then
        phi_u=alp1s*float(j-1)/float(nt1)   ! upper of a face
        phi_d=alp1s*float(j)/float(nt1)     ! lower of a face
       else
        phi_u=alp1s*float(nt1-1)/float(nt1)
     &       +float(j-nt1)*alp1s/float(nt1)/float(nt11)
        phi_d=alp1s*float(nt1-1)/float(nt1)
     &       +float(j-nt1+1)*alp1s/float(nt1)/float(nt11)
       endif

       if(j.eq.1) then   ! if face is upper most one

         ff_u(j)=Rp; R1_u=Rp

         si_d=-2.*(phi_d/phi1s)**3+3.*(phi_d/phi1s)**2         ! radius of a branch
         if(phi_d/phi1s.ge.1.0) si_d=1.
         R1_d=Rp-(Rp-Rd1)*si_d

         delR1_d=R1_d-Rd1

         c1d=phi_d
         ttd=-2.*(c1d/alp1s)**3+3.*(c1d/alp1s)**2   ! 0-1
         c2d=ttd**dv*c1d*phi2s/phi1s                ! dividing line
     &     +(1.-ttd**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c1d))
         x_d=(-tan(c1d)*(R1s+s1)+tan(c2d)*(R2s+s2))/(tan(c1d)+tan(c2d)) 
         z_d=-tan(c1d)*tan(c2d)*(R1s+R2s+s1+s2)/(tan(c1d)+tan(c2d))
         xc_d=R1s*cos(c1d)-R1s-s1
         zc_d=-R1s*sin(c1d)

         alx_d=SQRT((xc_d-x_d)**2+(zc_d-z_d)**2)
         ff_d(j)=HEIG(phi_d,alp1s,Rp,R1_d)              ! fy_d : y coord. of dividing line

         ratio=(Rp-ff_d(j))/Rp
         alt=(0.6*alx_d + 0.2*alx_d*ratio)+xx*alx_d       ! xx1    model2
         R1tmp=sqrt(R1_d**2-(alt-delR1_d)**2)
         if(ff_d(j).ge.R1tmp) mar_d(j)=1

       else

         si_u=-2.*(phi_u/phi1s)**3+3.*(phi_u/phi1s)**2         ! radius of a branch
         if(phi_u/phi1s.ge.1.0) si_u=1.
         R1_u=Rp-(Rp-Rd1)*si_u
         si_d=-2.*(phi_d/phi1s)**3+3.*(phi_d/phi1s)**2
         if(phi_d/phi1s.ge.1.0) si_d=1.
         R1_d=Rp-(Rp-Rd1)*si_d

         delR1_u=R1_u-Rd1
         delR1_d=R1_d-Rd1

         c1u=phi_u
         ttu=-2.*(c1u/alp1s)**3+3.*(c1u/alp1s)**2   ! 0-1
         c2u=ttu**dv*c1u*phi2s/phi1s               ! dividing line
     &     +(1.-ttu**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c1u))
         c1d=phi_d
         ttd=-2.*(c1d/alp1s)**3+3.*(c1d/alp1s)**2   ! 0-1
         c2d=ttd**dv*c1d*phi2s/phi1s               ! dividing line
     &     +(1.-ttd**dv)*atan(
     &   (R1s+s1+(-s1+s2)/2.)/(R2s+s2-(-s1+s2)/2.)*tan(c1d))
         x_u=(-tan(c1u)*(R1s+s1)+tan(c2u)*(R2s+s2))/(tan(c1u)+tan(c2u))  
         z_u=-tan(c1u)*tan(c2u)*(R1s+R2s+s1+s2)/(tan(c1u)+tan(c2u))
         x_d=(-tan(c1d)*(R1s+s1)+tan(c2d)*(R2s+s2))/(tan(c1d)+tan(c2d))
         z_d=-tan(c1d)*tan(c2d)*(R1s+R2s+s1+s2)/(tan(c1d)+tan(c2d))
         xc_u=R1s*cos(c1u)-R1s-s1
         zc_u=-R1s*sin(c1u)
         xc_d=R1s*cos(c1d)-R1s-s1
         zc_d=-R1s*sin(c1d)

         alx_u=SQRT((xc_u-x_u)**2+(zc_u-z_u)**2) 
         alx_d=SQRT((xc_d-x_d)**2+(zc_d-z_d)**2)

         ff_u(j)=HEIG(phi_u,alp1s,Rp,R1_u)              ! fy_d : y coord. of dividing line
         ff_d(j)=HEIG(phi_d,alp1s,Rp,R1_d)
         if(j.eq.nt1-1+nt11) ff_d(j)=0.0

         ratio=(Rp-ff_u(j))/Rp         ! xx1
         alt=(0.6*alx_u + 0.2*alx_u*ratio) + xx*alx_u    ! model2 
         R1tmp=sqrt(R1_u**2-(alt-delR1_u)**2)
         if(ff_u(j).ge.R1tmp) mar_u(j)=1
         ratio=(Rp-ff_d(j))/Rp         ! xx1
         alt=(0.6*alx_d + 0.2*alx_d*ratio) + xx*alx_d    ! model2
         R1tmp=sqrt(R1_d**2-(alt-delR1_d)**2)
         if(ff_d(j).ge.R1tmp) mar_d(j)=1

       endif

      enddo


      do j=1,nt1-1+nt11               ! right correction

       if(j.le.nt1-1) then
        phi_u=alp2s*float(j-1)/float(nt1)   ! upper of a face
        phi_d=alp2s*float(j)/float(nt1)     ! lower of a face
       else
        phi_u=alp2s*float(nt1-1)/float(nt1)
     &       +float(j-nt1)*alp2s/float(nt1)/float(nt11)
        phi_d=alp2s*float(nt1-1)/float(nt1)
     &       +float(j-nt1+1)*alp2s/float(nt1)/float(nt11)
       endif

       if(j.eq.1) then   ! if face is upper most one

         si_d=-2.*(phi_d/phi2s)**3+3.*(phi_d/phi2s)**2         ! radius of a branch
         if(phi_d/phi2s.ge.1.0) si_d=1.0
         R2_d=Rp-(Rp-Rd2)*si_d

         delR2_d=R2_d-Rd2

         c2d=phi_d
         ttd=-2.*(c2d/alp2s)**3+3.*(c2d/alp2s)**2   ! 0-1
         c1d=ttd**dv*c2d*phi1s/phi2s               ! dividing line
     &     +(1.-ttd**dv)*atan(
     &   (R2s+s2-(-s1+s2)/2.)/(R1s+s1+(-s1+s2)/2.)*tan(c2d))
         x_d=(-tan(c1d)*(R1s+s1)+tan(c2d)*(R2s+s2))/(tan(c1d)+tan(c2d))     ! point on dividing line
         z_d=-tan(c1d)*tan(c2d)*(R1s+R2s+s1+s2)/(tan(c1d)+tan(c2d))
         xc_d=R2s+s2-R2s*cos(c2d)                               ! point on center line of a branch
         zc_d=-R2s*sin(c2d)

         alx_d=SQRT((xc_d-x_d)**2+(zc_d-z_d)**2)  ! distance btw center of a branch & dividing line

         ratio=(Rp-ff_d(j))/Rp     ! xx2
         alt=(0.6*alx_d + 0.2*alx_d*ratio) + xx*alx_d   ! model2
         R2tmp=sqrt(R2_d**2-(alt-delR2_d)**2)
         if(ff_d(j).ge.R2tmp) mar_d(j)=1

       else

         si_u=-2.*(phi_u/phi2s)**3+3.*(phi_u/phi2s)**2         ! radius of a branch
         if(phi_u/phi2s.ge.1.0) si_u=1.0
         R2_u=Rp-(Rp-Rd2)*si_u
         si_d=-2.*(phi_d/phi2s)**3+3.*(phi_d/phi2s)**2
         if(phi_d/phi2s.ge.1.0) si_d=1.0
         R2_d=Rp-(Rp-Rd2)*si_d

         delR2_u=R2_u-Rd2
         delR2_d=R2_d-Rd2

         c2u=phi_u
         ttu=-2.*(c2u/alp2s)**3+3.*(c2u/alp2s)**2   ! 0-1
         c1u=ttu**dv*c2u*phi1s/phi2s               ! dividing line
     &     +(1.-ttu**dv)*atan(
     &   (R2s+s2-(-s1+s2)/2.)/(R1s+s1+(-s1+s2)/2.)*tan(c2u))
         c2d=phi_d
         ttd=-2.*(c2d/alp2s)**3+3.*(c2d/alp2s)**2   ! 0-1
         c1d=ttd**dv*c2d*phi1s/phi2s               ! dividing line
     &     +(1.-ttd**dv)*atan(
     &   (R2s+s2-(-s1+s2)/2.)/(R1s+s1+(-s1+s2)/2.)*tan(c2d))
         x_u=(-tan(c1u)*(R1s+s1)+tan(c2u)*(R2s+s2))/(tan(c1u)+tan(c2u))     ! point on dividing line
         z_u=-tan(c1u)*tan(c2u)*(R1s+R2s+s1+s2)/(tan(c1u)+tan(c2u))
         x_d=(-tan(c1d)*(R1s+s1)+tan(c2d)*(R2s+s2))/(tan(c1d)+tan(c2d))
         z_d=-tan(c1d)*tan(c2d)*(R1s+R2s+s1+s2)/(tan(c1d)+tan(c2d))
         xc_u=R2s+s2-R2s*cos(c2u)
         zc_u=-R2s*sin(c2u)
         xc_d=R2s+s2-R2s*cos(c2d)
         zc_d=-R2s*sin(c2d)

         alx_u=SQRT((xc_u-x_u)**2+(zc_u-z_u)**2)  ! distance btw center of a branch & dividing line
         alx_d=SQRT((xc_d-x_d)**2+(zc_d-z_d)**2)

         ratio=(Rp-ff_u(j))/Rp       ! xx2
         alt=(0.6*alx_u + 0.2*alx_u*ratio) + xx*alx_u    ! model2
         R2tmp=sqrt(R2_u**2-(alt-delR2_u)**2)
         if(ff_u(j).ge.R2tmp) mar_u(j)=1
         ratio=(Rp-ff_d(j))/Rp       ! xx2
         alt=(0.6*alx_d + 0.2*alx_d*ratio) + xx*alx_d    ! model2
         R2tmp=sqrt(R2_d**2-(alt-delR2_d)**2)
         if(ff_d(j).ge.R2tmp) mar_d(j)=1

       endif

      enddo

      return
      end

c***************** function HEIG *****************************
c     this function returns y coordinate of the dividing line
c     at a given phi
c*************************************************************
      FUNCTION HEIG(phi,alps,Rp,R)

      HEIG=R*( abs(1.-(phi/alps)**2) )**(0.4) 
       
      return
      end


c***************** function F1l *****************************
c     this function returns y coordinate of a point (upper-left)
c    
c*************************************************************
      FUNCTION F1l(Rp,phi,R,fy,alx,xs,j,nt1,nt11,iud,delR)

      DIMENSION aa(6,6),bb(6,1)

      n=6;np=6
      m=1;mp=1

      cli=2.0
      z=fy**2

      tt=(Rp-fy)/Rp  ! 0-1
      ratio=tt

      x1=(0.6*alx + 0.2*alx*ratio)         ! xx1
      if(x1.ge.0.8*alx) x1=0.8*alx
      y1=sqrt( abs(R**2-(x1-delR)**2) )
      yy1=-(x1-delR)/sqrt( abs(R**2-(x1-delR)**2) )
      yyy1=-R**2/(R**2-(x1-delR)**2)**(3./2.)

      IF(j.eq.nt1-1+nt11.and.iud.eq.2) THEN
       x2=0.98*alx
       y2=-cli*(x2-alx)*cos(phi)
       yy2=-cos(phi)*cli
       yyy2=0.0
      ELSE
       x2=0.98*alx
       y2=sqrt((cli*(x2-alx)*cos(phi))**2+z)
       yy2=cos(phi)**2*(cli)**2*(x2-alx)/
     &     sqrt((cos(phi)*cli*(x2-alx))**2+z)
       yyy2=cos(phi)**2*cli**2/sqrt((cos(phi)*cli*(x2-alx))**2+z)-
     &      (cos(phi)*cli)**4*(x2-alx)**2
     &     /((cos(phi)*cli*(x2-alx))**2+z)**(3./2.)
      ENDIF

      aa(1,1)=x1**5
      aa(1,2)=x1**4
      aa(1,3)=x1**3
      aa(1,4)=x1**2
      aa(1,5)=x1**1
      aa(1,6)=1.

      aa(2,1)=5.*x1**4
      aa(2,2)=4.*x1**3
      aa(2,3)=3.*x1**2
      aa(2,4)=2.*x1**1
      aa(2,5)=1.
      aa(2,6)=0.

      aa(3,1)=20.*x1**3
      aa(3,2)=12.*x1**2
      aa(3,3)=6.*x1**1
      aa(3,4)=2.
      aa(3,5)=0.
      aa(3,6)=0.

      aa(4,1)=x2**5
      aa(4,2)=x2**4
      aa(4,3)=x2**3
      aa(4,4)=x2**2
      aa(4,5)=x2**1
      aa(4,6)=1.

      aa(5,1)=5.*x2**4
      aa(5,2)=4.*x2**3
      aa(5,3)=3.*x2**2
      aa(5,4)=2.*x2**1
      aa(5,5)=1.
      aa(5,6)=0.

      aa(6,1)=20.*x2**3
      aa(6,2)=12.*x2**2
      aa(6,3)=6.*x2**1
      aa(6,4)=2.
      aa(6,5)=0.
      aa(6,6)=0.

      bb(1,1)=y1
      bb(2,1)=yy1
      bb(3,1)=yyy1
      bb(4,1)=y2
      bb(5,1)=yy2
      bb(6,1)=yyy2

      call gaussj(aa,n,np,bb,m,mp)

      a5=bb(1,1)
      a4=bb(2,1)
      a3=bb(3,1)
      a2=bb(4,1)
      a1=bb(5,1)
      a0=bb(6,1)

      Fa=sqrt(abs(R**2-(xs-delR)**2))
      Fb=a5*xs**5+a4*xs**4+a3*xs**3+a2*xs**2+a1*xs+a0
      if(j.eq.nt1-1+nt11.and.iud.eq.2) then
       Fc=-cli*(xs-alx)*cos(phi)
      else
       Fc=sqrt(abs((cli*(xs-alx)*cos(phi))**2+z))
      endif

      if(xs.gt.x1.and.xs.le.x2) then
       dydx=5.*a5*xs**4+4.*a4*xs**3+3.*a3*xs**2+2.*a2*xs+a1
c       if(dydx.gt.0.0) print *,'left-up',alx/R,fy/R,x1/alx,dydx
      endif

      if(xs.le.x1)              F1l=Fa
      if(xs.gt.x1.and.xs.le.x2) F1l=Fb
      if(xs.gt.x2)              F1l=Fc

      return
      end

c***************** function FFL ******************************
c     this function returns y coordinate of a point (upper-left)
c
c*************************************************************
      FUNCTION FFL(Rp,phi1,phi2,R1,R2,fy,alx1,alx2,
     &             xs,j,delR1,delR2)

      DIMENSION aa(7,7),bb(7,1)

      n=7;np=7
      m=1;mp=1

      tt=(Rp-fy)/Rp  ! 0-1
      ratio=tt

      x1o=(0.6*alx1 + 0.2*alx1*ratio)      ! xx1    ! x1o is on the plane with angle phi         
      y1=sqrt(R1**2-(x1o-delR1)**2)
      yy1=-(x1o-delR1)/sqrt(R1**2-(x1o-delR1)**2)/cos(phi1)
      yyy1=-R1**2/(R1**2-(x1o-delR1)**2)**(3./2.)/cos(phi1)**2

      beta=(R1-fy)/(R1-R2)
      if(fy.lt.R2) beta=1.0
      if(R1.eq.fy) beta=0.0
      x2o=0.5*alx2*beta

      y2=sqrt(R2**2-(delR2-x2o)**2)
      yy2=-(delR2-x2o)/sqrt(R2**2
     &    -(delR2-x2o)**2)/cos(phi2)
      yyy2=-R2**2/(R2**2-(delR2-x2o)**2)**(3./2.)
     &     /cos(phi2)**2

      x1=x1o*cos(phi1)
      x2=alx1*cos(phi1)+alx2*cos(phi2)-x2o*cos(phi2)
      x3=alx1*cos(phi1)

c---- set new fy ----------
      fy1=fy

      alx1p=alx1-x1o
      alx2p=alx2-x2o 

      R1p=sqrt(R1**2-(x1o-delR1)**2)
      R2pp=sqrt(R2**2-delR2**2)
      R2p=sqrt(R2**2-(x2o-delR2)**2)

      w=alx1p/(alx1p+alx2p)
      fy2=R1p-(R1p-R2p)*(-2.*w**3+3.*w**2)

      beta=(R1-fy1)/(R1-R2)  ! 0-1
      if(fy.lt.R2) beta=1.0
      if(fy1.eq.R1) beat=0.0

      fyt=(1.-beta)*fy2+beta*fy1
c--------------------------

      aa(1,1)=x1**6
      aa(1,2)=x1**5
      aa(1,3)=x1**4
      aa(1,4)=x1**3
      aa(1,5)=x1**2
      aa(1,6)=x1**1
      aa(1,7)=1.

      aa(2,1)=6.*x1**5
      aa(2,2)=5.*x1**4
      aa(2,3)=4.*x1**3
      aa(2,4)=3.*x1**2
      aa(2,5)=2.*x1**1
      aa(2,6)=1.
      aa(2,7)=0.

      aa(3,1)=30.*x1**4
      aa(3,2)=20.*x1**3
      aa(3,3)=12.*x1**2
      aa(3,4)=6.*x1**1
      aa(3,5)=2.
      aa(3,6)=0.
      aa(3,7)=0.

      aa(4,1)=x2**6
      aa(4,2)=x2**5
      aa(4,3)=x2**4
      aa(4,4)=x2**3
      aa(4,5)=x2**2
      aa(4,6)=x2**1
      aa(4,7)=1.

      aa(5,1)=6.*x2**5
      aa(5,2)=5.*x2**4
      aa(5,3)=4.*x2**3
      aa(5,4)=3.*x2**2
      aa(5,5)=2.*x2**1
      aa(5,6)=1.
      aa(5,7)=0.

      aa(6,1)=30.*x2**4
      aa(6,2)=20.*x2**3
      aa(6,3)=12.*x2**2
      aa(6,4)=6.*x2**1
      aa(6,5)=2.
      aa(6,6)=0.
      aa(6,7)=0.

      aa(7,1)=x3**6
      aa(7,2)=x3**5
      aa(7,3)=x3**4
      aa(7,4)=x3**3
      aa(7,5)=x3**2
      aa(7,6)=x3**1
      aa(7,7)=1.

      bb(1,1)=y1
      bb(2,1)=yy1
      bb(3,1)=yyy1
      bb(4,1)=y2
      bb(5,1)=yy2
      bb(6,1)=yyy2
      bb(7,1)=fyt

      call gaussj(aa,n,np,bb,m,mp)

      a6=bb(1,1)
      a5=bb(2,1)
      a4=bb(3,1)
      a3=bb(4,1)
      a2=bb(5,1)
      a1=bb(6,1)
      a0=bb(7,1)

      Fa=sqrt(abs(R1**2-(xs-delR1)**2))
      xst=xs*cos(phi1)
      Fb=a6*xst**6+a5*xst**5+a4*xst**4
     &  +a3*xst**3+a2*xst**2+a1*xst+a0

      if(xs.gt.x1o)     FFL=Fb
      if(xs.le.x1o)     FFL=Fa
c      if(Fb.gt.R1.and.xs.gt.x1o) print *,'FFL',Fb/R1,R1,alx1,R2,alx2,
c     &                           fyt,cos(phi1),cos(phi2),x1

      return
      end

c***************** function F1r *****************************
c     this function returns y coordinate of a point (upper-right)
c
c*************************************************************
      FUNCTION F1r(Rp,phi,R,fy,alx,xs,j,nt1,nt11,iud,delR)

      DIMENSION aa(6,6),bb(6,1)

      n=6;np=6
      m=1;mp=1

      cli=2.0
      z=fy**2

      tt=(Rp-fy)/Rp  ! 0-1
      ratio=tt

      x1=(0.6*alx+0.2*alx*ratio)        ! xx2
      if(x1.ge.0.8*alx) x1=0.8*alx
      y1=sqrt( abs(R**2-(x1-delR)**2) )
      yy1=-(x1-delR)/sqrt( abs(R**2-(x1-delR)**2) )
      yyy1=-R**2/(R**2-(x1-delR)**2)**(3./2.)

      IF(j.eq.nt1-1+nt11.and.iud.eq.2) THEN
       x2=0.98*alx
       y2=-cli*(x2-alx)*cos(phi)
       yy2=-cos(phi)*cli
       yyy2=0.0
      ELSE
       x2=0.98*alx
       y2=sqrt((cli*(x2-alx)*cos(phi))**2+z)
       yy2=cos(phi)**2*(cli)**2*(x2-alx)/
     &     sqrt((cos(phi)*cli*(x2-alx))**2+z)
       yyy2=cos(phi)**2*cli**2/sqrt((cos(phi)*cli*(x2-alx))**2+z)-
     &      (cos(phi)*cli)**4*(x2-alx)**2
     &     /((cos(phi)*cli*(x2-alx))**2+z)**(3./2.)
      ENDIF

      aa(1,1)=x1**5
      aa(1,2)=x1**4
      aa(1,3)=x1**3
      aa(1,4)=x1**2
      aa(1,5)=x1**1
      aa(1,6)=1.

      aa(2,1)=5.*x1**4
      aa(2,2)=4.*x1**3
      aa(2,3)=3.*x1**2
      aa(2,4)=2.*x1**1
      aa(2,5)=1.
      aa(2,6)=0.

      aa(3,1)=20.*x1**3
      aa(3,2)=12.*x1**2
      aa(3,3)=6.*x1**1
      aa(3,4)=2.
      aa(3,5)=0.
      aa(3,6)=0.

      aa(4,1)=x2**5
      aa(4,2)=x2**4
      aa(4,3)=x2**3
      aa(4,4)=x2**2
      aa(4,5)=x2**1
      aa(4,6)=1.

      aa(5,1)=5.*x2**4
      aa(5,2)=4.*x2**3
      aa(5,3)=3.*x2**2
      aa(5,4)=2.*x2**1
      aa(5,5)=1.
      aa(5,6)=0.

      aa(6,1)=20.*x2**3
      aa(6,2)=12.*x2**2
      aa(6,3)=6.*x2**1
      aa(6,4)=2.
      aa(6,5)=0.
      aa(6,6)=0.

      bb(1,1)=y1
      bb(2,1)=yy1
      bb(3,1)=yyy1
      bb(4,1)=y2
      bb(5,1)=yy2
      bb(6,1)=yyy2

      call gaussj(aa,n,np,bb,m,mp)

      a5=bb(1,1)
      a4=bb(2,1)
      a3=bb(3,1)
      a2=bb(4,1)
      a1=bb(5,1)
      a0=bb(6,1)

      Fa=sqrt(abs(R**2-(xs-delR)**2))
      Fb=a5*xs**5+a4*xs**4+a3*xs**3+a2*xs**2+a1*xs+a0
      if(j.eq.nt1-1+nt11.and.iud.eq.2) then
       Fc=-cli*(xs-alx)*cos(phi)
      else
       Fc=sqrt(abs((cli*(xs-alx)*cos(phi))**2+z))
      endif

      if(xs.gt.x1.and.xs.le.x2) then
       dydx=5.*a5*xs**4+4.*a4*xs**3+3.*a3*xs**2+2.*a2*xs+a1
c       if(dydx.gt.0.0) print *,'right-up',alx/R,fy/R,x1/alx,dydx
      endif

      if(xs.le.x1)              F1r=Fa
      if(xs.gt.x1.and.xs.le.x2) F1r=Fb
      if(xs.gt.x2)              F1r=Fc

      return
      end


c***************** function FFR ******************************
c     this function returns y coordinate of a point (upper-right)
c
c*************************************************************
      FUNCTION FFR(Rp,phi2,phi1,R2,R1,fy,alx2,alx1,
     &             xs,delR1,delR2)

      DIMENSION aa(7,7),bb(7,1)

      n=7;np=7
      m=1;mp=1

      tt=(Rp-fy)/Rp  ! 0-1
      ratio=tt

      x1o=(0.6*alx1 + 0.2*alx1*ratio)       ! xx1
      y1=sqrt(R1**2-(x1o-delR1)**2)
      yy1=-(x1o-delR1)/sqrt(R1**2-(x1o-delR1)**2)/cos(phi1)
      yyy1=-R1**2/(R1**2-(x1o-delR1)**2)**(3./2.)/cos(phi1)**2

      beta=(R1-fy)/(R1-R2)
      if(fy.lt.R2) beta=1.0
      if(fy.eq.R1) beta=1.0
      x2o=0.5*alx2*beta

      y2=sqrt(R2**2-(delR2-x2o)**2)
      yy2=-(delR2-x2o)/sqrt(R2**2
     &    -(delR2-x2o)**2)/cos(phi2)
      yyy2=-R2**2/(R2**2-(delR2-x2o)**2)**(3./2.)
     &     /cos(phi2)**2

      x1=x1o*cos(phi1)
      x2=alx1*cos(phi1)+alx2*cos(phi2)-x2o*cos(phi2)
      x3=alx1*cos(phi1)

c---- set new fy ----------
      fy1=fy

      alx1p=alx1-x1o
      alx2p=alx2-x2o

      R1p=sqrt(R1**2-(x1o-delR1)**2)
      R2pp=sqrt(R2**2-delR2**2)
      R2p=sqrt(R2**2-(x2o-delR2)**2)

      w=alx1p/(alx1p+alx2p)
      fy2=R1p-(R1p-R2p)*(-2.*w**3+3.*w**2)

      beta=(R1-fy1)/(R1-R2)  ! 0-1
      if(fy1.lt.R2) beta=1.0
      if(fy1.eq.R1) beta=0.0

      fyt=(1.-beta)*fy2+beta*fy1
c--------------------------

      aa(1,1)=x1**6
      aa(1,2)=x1**5
      aa(1,3)=x1**4
      aa(1,4)=x1**3
      aa(1,5)=x1**2
      aa(1,6)=x1**1
      aa(1,7)=1.

      aa(2,1)=6.*x1**5
      aa(2,2)=5.*x1**4
      aa(2,3)=4.*x1**3
      aa(2,4)=3.*x1**2
      aa(2,5)=2.*x1**1
      aa(2,6)=1.
      aa(2,7)=0.

      aa(3,1)=30.*x1**4
      aa(3,2)=20.*x1**3
      aa(3,3)=12.*x1**2
      aa(3,4)=6.*x1**1
      aa(3,5)=2.
      aa(3,6)=0.
      aa(3,7)=0.

      aa(4,1)=x2**6
      aa(4,2)=x2**5
      aa(4,3)=x2**4
      aa(4,4)=x2**3
      aa(4,5)=x2**2
      aa(4,6)=x2**1
      aa(4,7)=1.

      aa(5,1)=6.*x2**5
      aa(5,2)=5.*x2**4
      aa(5,3)=4.*x2**3
      aa(5,4)=3.*x2**2
      aa(5,5)=2.*x2**1
      aa(5,6)=1.
      aa(5,7)=0.

      aa(6,1)=30.*x2**4
      aa(6,2)=20.*x2**3
      aa(6,3)=12.*x2**2
      aa(6,4)=6.*x2**1
      aa(6,5)=2.
      aa(6,6)=0.
      aa(6,7)=0.

      aa(7,1)=x3**6
      aa(7,2)=x3**5
      aa(7,3)=x3**4
      aa(7,4)=x3**3
      aa(7,5)=x3**2
      aa(7,6)=x3**1
      aa(7,7)=1.

      bb(1,1)=y1
      bb(2,1)=yy1
      bb(3,1)=yyy1
      bb(4,1)=y2
      bb(5,1)=yy2
      bb(6,1)=yyy2
      bb(7,1)=fyt

      call gaussj(aa,n,np,bb,m,mp)

      a6=bb(1,1)
      a5=bb(2,1)
      a4=bb(3,1)
      a3=bb(4,1)
      a2=bb(5,1)
      a1=bb(6,1)
      a0=bb(7,1)

      xst=alx2*cos(phi2)+alx1*cos(phi1)-xs*cos(phi2)
      Fb=sqrt(abs(R2**2-(xs-delR2)**2))
      Fa=a6*xst**6+a5*xst**5+a4*xst**4
     &  +a3*xst**3+a2*xst**2+a1*xst+a0

      if(xs.le.x2o)    FFR=Fb
      if(xs.gt.x2o)    FFR=Fa

      return
      end


c***************** function F2l *******************************
c     this function returns y coordinate of a point (lower-left)
c    
c*************************************************************
      FUNCTION F2l(R,alx,xs,R1s,xxb,phi,alp,alps,j,iud,s1)

      DIMENSION aa(5,5),bb(5,1)
      DIMENSION aaa(6,6),bbb(6,1)

      n=5;np=5
      m=1;mp=1

      cli=2.0
      xt=R1s*cos(phi)-R1s-s1
      xb=-alx+abs(xxb-xt)/cos(phi)

      tt=(phi-alps)/(alp-alps)   ! 0-1
c      ratio=-2.*tt**3+3.*tt**2
      ratio=tt

      x1=0.8*alx + 0.15*alx*ratio                   ! circular shape end
      y1=sqrt( abs(R**2-x1**2) )
      yy1=-x1/sqrt( abs(R**2-x1**2) )
      yyy1=-R**2/(R**2-x1**2)**(3./2.)
      x2=0.98*alx
      y2=sqrt(cos(phi)**2*abs((cli*(x2-alx-xb))**2-(cli*xb)**2))
      yy2=cos(phi)*(cli)**2*(x2-alx-xb)/
     &    sqrt(abs((cli*(x2-alx-xb))**2-(cli*xb)**2))

      aa(1,1)=x1**4
      aa(1,2)=x1**3
      aa(1,3)=x1**2
      aa(1,4)=x1**1
      aa(1,5)=1.

      aa(2,1)=4.*x1**3
      aa(2,2)=3.*x1**2
      aa(2,3)=2.*x1**1
      aa(2,4)=1.
      aa(2,5)=0.

      aa(3,1)=12.*x1**2
      aa(3,2)=6.*x1**1
      aa(3,3)=2.
      aa(3,4)=0.
      aa(3,5)=0.

      aa(4,1)=x2**4
      aa(4,2)=x2**3
      aa(4,3)=x2**2
      aa(4,4)=x2**1
      aa(4,5)=1.

      aa(5,1)=4.*x2**3
      aa(5,2)=3.*x2**2
      aa(5,3)=2.*x2**1
      aa(5,4)=1.
      aa(5,5)=0.

      bb(1,1)=y1
      bb(2,1)=yy1
      bb(3,1)=yyy1
      bb(4,1)=y2
      bb(5,1)=yy2

      call gaussj(aa,n,np,bb,m,mp)

      a4=bb(1,1)
      a3=bb(2,1)
      a2=bb(3,1)
      a1=bb(4,1)
      a0=bb(5,1)

      Fa=sqrt(abs(R**2-xs**2))
      Fb=a4*xs**4+a3*xs**3+a2*xs**2+a1*xs+a0
      IF(j.eq.1.and.iud.eq.1) THEN
       Fc=-cli*(xs-alx)*cos(phi)
      ELSE
       Fc=sqrt(cos(phi)**2*abs((cli*(xs-alx-xb))**2-(cli*xb)**2))
      ENDIF

      Fd=R*sqrt(abs(1.0-(xs/alx)**2))

      if(xs.gt.x1.and.xs.le.x2) then
       dydx=4.*a4*xs**3+3.*a3*xs**2+2.*a2*xs+a1
c       if(dydx.gt.0.0) print *,'left-dn'
      endif

      if(xs.le.x1)              F2l=Fa*(1.-ratio)+Fd*ratio
      if(xs.gt.x1.and.xs.le.x2) F2l=Fb*(1.-ratio)+Fd*ratio
      if(xs.gt.x2)              F2l=Fc*(1.-ratio)+Fd*ratio
 


      IF(j.eq.1.and.iud.eq.1) THEN

       n=6;np=6
       m=1;mp=1

       cli=2.0

       x1=0.8*alx                          ! circular shape end
       y1=sqrt( abs(R**2-x1**2) )
       yy1=-x1/sqrt( abs(R**2-x1**2) )
       yyy1=-R**2/(R**2-x1**2)**(3./2.)
       x2=0.98*alx
       y2=-cli*(x2-alx)*cos(phi)
       yy2=-cos(phi)*cli
       yyy2=0.0

       aaa(1,1)=x1**5
       aaa(1,2)=x1**4
       aaa(1,3)=x1**3
       aaa(1,4)=x1**2
       aaa(1,5)=x1**1
       aaa(1,6)=1.

       aaa(2,1)=5.*x1**4
       aaa(2,2)=4.*x1**3
       aaa(2,3)=3.*x1**2
       aaa(2,4)=2.*x1**1
       aaa(2,5)=1.
       aaa(2,6)=0.

       aaa(3,1)=20.*x1**3
       aaa(3,2)=12.*x1**2
       aaa(3,3)=6.*x1**1
       aaa(3,4)=2.
       aaa(3,5)=0.
       aaa(3,6)=0.

       aaa(4,1)=x2**5
       aaa(4,2)=x2**4
       aaa(4,3)=x2**3
       aaa(4,4)=x2**2
       aaa(4,5)=x2**1
       aaa(4,6)=1.

       aaa(5,1)=5.*x2**4
       aaa(5,2)=4.*x2**3
       aaa(5,3)=3.*x2**2
       aaa(5,4)=2.*x2**1
       aaa(5,5)=1.
       aaa(5,6)=0.

       aaa(6,1)=20.*x2**3
       aaa(6,2)=12.*x2**2
       aaa(6,3)=6.*x2**1
       aaa(6,4)=2.
       aaa(6,5)=0.
       aaa(6,6)=0.

       bbb(1,1)=y1
       bbb(2,1)=yy1
       bbb(3,1)=yyy1
       bbb(4,1)=y2
       bbb(5,1)=yy2
       bbb(6,1)=yyy2

       call gaussj(aaa,n,np,bbb,m,mp)

       a5=bbb(1,1)
       a4=bbb(2,1)
       a3=bbb(3,1)
       a2=bbb(4,1)
       a1=bbb(5,1)
       a0=bbb(6,1)

       Fa=sqrt(abs(R**2-xs**2))
       Fb=a5*xs**5+a4*xs**4+a3*xs**3+a2*xs**2+a1*xs+a0
       Fc=-cli*(xs-alx)*cos(phi)

       Fd=R*sqrt(abs(1.0-(xs/alx)**2))

       if(xs.gt.x1.and.xs.le.x2) then
        dydx=5.*a5*xs**4+4.*a4*xs**3+3.*a3*xs**2+2.*a2*xs+a1
c        if(dydx.gt.0.0) print *,'left-dn'
       endif

       if(xs.le.x1)              F2l=Fa
       if(xs.gt.x1.and.xs.le.x2) F2l=Fb
       if(xs.gt.x2)              F2l=Fc

      ENDIF



      return
      end


c***************** function F2r *******************************
c     this function returns y coordinate of a point (lower-right)
c
c*************************************************************
      FUNCTION F2r(R,alx,xs,R2s,xxb,phi,alp,alps,j,iud,s2)

      DIMENSION aa(5,5),bb(5,1)
      DIMENSION aaa(6,6),bbb(6,1)

      n=5;np=5
      m=1;mp=1

      cli=2.0
      xt=R2s+s2-R2s*cos(phi)
      xb=-alx+abs(xxb-xt)/cos(phi)

      tt=(phi-alps)/(alp-alps)   ! 0-1
c      ratio=-2.*tt**3+3.*tt**2
      ratio=tt

      x1=0.8*alx + 0.15*alx*ratio                         ! circular shape end
      y1=sqrt( abs(R**2-x1**2) )
      yy1=-x1/sqrt( abs(R**2-x1**2) )
      yyy1=-R**2/(R**2-x1**2)**(3./2.)
      x2=0.98*alx
      y2=sqrt(cos(phi)**2*abs((cli*(x2-alx-xb))**2-(cli*xb)**2))
      yy2=cos(phi)*(cli)**2*(x2-alx-xb)/
     &    sqrt(abs((cli*(x2-alx-xb))**2-(cli*xb)**2))
      yyy2=cos(phi)*cli**2/
     &     sqrt(abs((cli*(x2-alx-xb))**2-(cli*xb)**2))
     &    -cos(phi)**4*cli**4*(x2-alx-xb)**2/
     &     (cos(phi)**2*abs((cli*(x2-alx-xb))**2-(cli*xb)**2))**(-1.5)

      aa(1,1)=x1**4
      aa(1,2)=x1**3
      aa(1,3)=x1**2
      aa(1,4)=x1**1
      aa(1,5)=1.

      aa(2,1)=4.*x1**3
      aa(2,2)=3.*x1**2
      aa(2,3)=2.*x1**1
      aa(2,4)=1.
      aa(2,5)=0.

      aa(3,1)=12.*x1**2
      aa(3,2)=6.*x1**1
      aa(3,3)=2.
      aa(3,4)=0.
      aa(3,5)=0.

      aa(4,1)=x2**4
      aa(4,2)=x2**3
      aa(4,3)=x2**2
      aa(4,4)=x2**1
      aa(4,5)=1.

      aa(5,1)=4.*x2**3
      aa(5,2)=3.*x2**2
      aa(5,3)=2.*x2**1
      aa(5,4)=1.
      aa(5,5)=0.

      bb(1,1)=y1
      bb(2,1)=yy1
      bb(3,1)=yyy1
      bb(4,1)=y2
      bb(5,1)=yy2

      call gaussj(aa,n,np,bb,m,mp)

      a4=bb(1,1)
      a3=bb(2,1)
      a2=bb(3,1)
      a1=bb(4,1)
      a0=bb(5,1)

      Fa=sqrt(abs(R**2-xs**2))
      Fb=a4*xs**4+a3*xs**3+a2*xs**2+a1*xs+a0
      IF(j.eq.1.and.iud.eq.1) THEN
       Fc=-cli*(xs-alx)*cos(phi)
      ELSE
       Fc=sqrt(cos(phi)**2*abs((cli*(xs-alx-xb))**2-(cli*xb)**2))
      ENDIF

      Fd=R*sqrt(abs(1.0-(xs/alx)**2))

      if(xs.gt.x1.and.xs.le.x2) then
       dydx=4.*a4*xs**3+3.*a3*xs**2+2.*a2*xs+a1
c       if(dydx.gt.0.0) print *,'right-dn'
      endif

      if(xs.le.x1)              F2r=Fa*(1.-ratio)+Fd*ratio
      if(xs.gt.x1.and.xs.le.x2) F2r=Fb*(1.-ratio)+Fd*ratio
      if(xs.gt.x2)              F2r=Fc*(1.-ratio)+Fd*ratio


      IF(j.eq.1.and.iud.eq.1) THEN

       n=6;np=6
       m=1;mp=1

       cli=2.0

       x1=0.8*alx                          ! circular shape end
       y1=sqrt( abs(R**2-x1**2) )
       yy1=-x1/sqrt( abs(R**2-x1**2) )
       yyy1=-R**2/(R**2-x1**2)**(3./2.)
       x2=0.98*alx
       y2=-cli*(x2-alx)*cos(phi)
       yy2=-cos(phi)*cli
       yyy2=0.0

       aaa(1,1)=x1**5
       aaa(1,2)=x1**4
       aaa(1,3)=x1**3
       aaa(1,4)=x1**2
       aaa(1,5)=x1**1
       aaa(1,6)=1.

       aaa(2,1)=5.*x1**4
       aaa(2,2)=4.*x1**3
       aaa(2,3)=3.*x1**2
       aaa(2,4)=2.*x1**1
       aaa(2,5)=1.
       aaa(2,6)=0.

       aaa(3,1)=20.*x1**3
       aaa(3,2)=12.*x1**2
       aaa(3,3)=6.*x1**1
       aaa(3,4)=2.
       aaa(3,5)=0.
       aaa(3,6)=0.

       aaa(4,1)=x2**5
       aaa(4,2)=x2**4
       aaa(4,3)=x2**3
       aaa(4,4)=x2**2
       aaa(4,5)=x2**1
       aaa(4,6)=1.

       aaa(5,1)=5.*x2**4
       aaa(5,2)=4.*x2**3
       aaa(5,3)=3.*x2**2
       aaa(5,4)=2.*x2**1
       aaa(5,5)=1.
       aaa(5,6)=0.

       aaa(6,1)=20.*x2**3
       aaa(6,2)=12.*x2**2
       aaa(6,3)=6.*x2**1
       aaa(6,4)=2.
       aaa(6,5)=0.
       aaa(6,6)=0.

       bbb(1,1)=y1
       bbb(2,1)=yy1
       bbb(3,1)=yyy1
       bbb(4,1)=y2
       bbb(5,1)=yy2
       bbb(6,1)=yyy2

       call gaussj(aaa,n,np,bbb,m,mp)

       a5=bbb(1,1)
       a4=bbb(2,1)
       a3=bbb(3,1)
       a2=bbb(4,1)
       a1=bbb(5,1)
       a0=bbb(6,1)

       Fa=sqrt(abs(R**2-xs**2))
       Fb=a5*xs**5+a4*xs**4+a3*xs**3+a2*xs**2+a1*xs+a0
       Fc=-cli*(xs-alx)*cos(phi)

       Fd=R*sqrt(abs(1.0-(xs/alx)**2))

       if(xs.gt.x1.and.xs.le.x2) then
        dydx=5.*a5*xs**4+4.*a4*xs**3+3.*a3*xs**2+2.*a2*xs+a1
c        if(dydx.gt.0.0) print *,'right-dn'
       endif

       if(xs.le.x1)              F2r=Fa
       if(xs.gt.x1.and.xs.le.x2) F2r=Fb
       if(xs.gt.x2)              F2r=Fc

      ENDIF

      return
      end
