      Dimension sy(110,110),sp(110,110),w(250,250),wp(110,110)
	Dimension u(110,110),v(110,110),te(110,110),tp(110,110)
	
      Dimension hti(110,110),ffi(110,110)
	Dimension ht1(110,110),ff1(110,110),be1(110,110),ans1(110,110)
	Dimension ht3(110,110),ff3(110,110),be3(110,110),ans3(110,110)
	Dimension ht5(110,110),ff5(110,110),be5(110,110),ans5(110,110)
	Dimension ht7(110,110),ff7(110,110),be7(110,110),ans7(110,110)
	Dimension ht9(110,110),ff9(110,110),be9(110,110),ans9(110,110)

	Dimension FFIbar(5),HTIbar(5),BEbar(5),Sbar(5)

	Dimension a(6,6),xx(20)

	real sy,sp,w,wp,u,v,te,tp,pr,ra
 	ge=10.**(-5)

      pr =.7
	ra =10.**(5)
	qw =5.
 	qs =5.
	qt =5.

	n =100
	g1=.5

	ic1 =n/4+1
	ic2 =ic1+n/2
	jc1 =n/2-2+n/4
	jc2 =n/2+1+2+n/4
		   
	dy =1./ n
	write(*,*)ic1,ic2,jc1,jc2
c    	'=======================================
	w=0.
	wp=0.
	u=0.
	v=0.
      sp=0.
  	sy=0.
	te=0.
	tp=0.
	ffi=0.
	hti=0.
c	'================ teta=1 ======================
	do i = ic1 , ic2
	do j = jc1, jc2
	tp(i,j) = 1.
	te(i,j) = 1.
	end do
	end do
c    '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c    '$$$$$     START          START       $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c    '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
	famoor=0
1000	famoor = famoor + 1
	if (famoor/200.eq.int(famoor/200)) then
	write(*,354)famoor,ii,jj,
     /wdif,sydif,tedif,wp(ii,jj),sp(ii,jj),tp(ii,jj)
354	format(">>",f7.0,i3,i3,3e10.2," ***",3e10.2)
	end if

c	 '===sy=======sy====sy==============
      
	do j = 2, n
	do i = 2 , n
	If ((i .ge. ic1) .And. (i .le. ic2)) Then
	If ((j .ge. jc1) .And. (j .le. jc2)) Then
	goto 2
	End If
	End If
	b1=-wp(i,j)
 	b2=0
	b31=(sp(i-1,j)+sp(i+1,j))/(dy**2)
	b32=(sp(i,j-1)+sp(i,j+1))/(dy**2)
	b3=(b31+b32)
	
	a1=0.
	a2=0.
	a3=(-2./(dy**2)-2./(dy**2))
	
     	sy(i,j)=sp(i,j)+g1*(-sp(i,j)-(b1+b2+b3)/(a1+a2+a3))
2     end do
	end do

c========  boundrey sy sy  ===============

c     '======uu===vv====uu====vv=========
	do i = 2,n
	do j = 2,n
	If ((i .ge. ic1) .And. (i .le. ic2)) Then
	If ((j .ge. jc1) .And. (j .le. jc2)) Then
	GoTo 3
	End If
	End If

	u(i,j)=(sy(i,j+1)-sy(i,j-1))/(2.*dy)
	v(i,j)=-(sy(i+1,j)-sy(i-1,j))/(2.*dy)

3	end do
	end do

c      '=====boundrey uu vv ==============
 
c       '======te ======te =====te=========
      do j = 2,n
	do i = 2,n
	If ((i .ge. ic1) .And. (i .le. ic2)) Then
	If ((j .ge. jc1) .And. (j .le. jc2)) Then
	GoTo 4
	End If
	End If
	b1=u(i,j)*(-tp(i-1,j)+tp(i+1,j))/(2.*dy)
 	b2=v(i,j)*(-tp(i,j-1)+tp(i,j+1))/(2.*dy)
	b31=(tp(i-1,j)+tp(i+1,j))/(dy**2)
	b32=(tp(i,j-1)+tp(i,j+1))/(dy**2)
	b3=-(b31+b32)
	
	a1=0.
	a2=0.
	a3=-(-2./(dy**2)-2./(dy**2))
	
	te(i,j)=tp(i,j)+g1*(-tp(i,j)-(b1+b2+b3)/(a1+a2+a3))

4	end do
	end do

c     '========boundrey teta=============

	do i=1,n+1
	te(i,1)=(18.*te(i,1+1)-9.*te(i,1+2)+2.*te(i,1+3))/(+11.)
	te(i,n+1)=(18.*te(i,n+1-1)-9.*te(i,n+1-2)+2.*te(i,n+1-3))/(+11.)
	end do

c      '======ww====ww======ww============
      do j = 2,n
	do i = 2,n
	If ((i .ge. ic1) .And. (i .le. ic2)) Then
	If ((j .ge. jc1) .And. (j .le. jc2)) Then
	GoTo 5
	End If
	End If
 	b1=u(i,j)*(-wp(i-1,j)+wp(i+1,j))/(2.*dy)
	b2=v(i,j)*(-wp(i,j-1)+wp(i,j+1))/(2.*dy)
	b31=(wp(i-1,j)+wp(i+1,j))/(dy**2)
	b32=(wp(i,j-1)+wp(i,j+1))/(dy**2)
	b3=-pr*(b31+b32)
	b4=ra*pr*(-tp(i-1,j)+tp(i+1,j))/(2.*dy)
	a1=0.
	a2=0.
	a3=-pr*(-2./(dy**2)-2./(dy**2))
	a4=0.
	w(i,j)=wp(i,j)+g1*(-wp(i,j)-(b1+b2+b3+b4)/(a1+a2+a3+a4))
5	end do
	end do
c	'======boundrey ww ww =============
  	do i = 2, n
c 	'forward
	j=1
  	w(i,1)=-1./(2.*dy**2.)*(sy(i,1+2)-8.*sy(i,1+1)+7.*sy(i,1))
 	w(1,i)=-1./(2.*dy**2.)*(sy(1+2,i)-8.*sy(1+1,i)+7.*sy(1,i))
c	'back ward
	j=n+1
	w(i,n+1)=-1./(2*dy**2.)*(sy(i,n+1-2)-8.*sy(i,n+1-1)+7.*sy(i,n+1))
	w(n+1,i)=-1./(2*dy**2.)*(sy(n+1-2,i)-8.*sy(n+1-1,i)+7.*sy(n+1,i))
	end do

      do i = ic1,ic2
c	'back ward
	w(i,jc1)=-1./(2.*dy**2)*(sy(i,jc1-2)-8.*sy(i,jc1-1)+7.*sy(i,jc1))
c	'forward
	w(i,jc2)=-1./(2.*dy**2)*(sy(i,jc2+2)-8.*sy(i,jc2+1)+7.*sy(i,jc2))
	end do

      do j = jc1+1,jc2-1
c	'back ward
	w(ic1,j)=-1./(2.*dy**2)*(sy(ic1-2,j)-8.*sy(ic1-1,j)+7.*sy(ic1,j))
c	'forward
	w(ic2,j)=-1./(2.*dy**2)*(sy(ic2+2,j)-8.*sy(ic2+1,j)+7.*sy(ic2,j))
	end do



    	w(ic1,jc1)=w(ic1,jc1)
     /-10./(2*dy**2.)*(sy(ic1-2,jc1)-8.*sy(ic1-1,jc1)+7.*sy(ic1,jc1))
      w(ic1,jc2)=w(ic1,jc2)
     /-10./(2*dy**2.)*(sy(ic1-2,jc2)-8.*sy(ic1-1,jc2)+7.*sy(ic1,jc2))
      w(ic2,jc1)=w(ic2,jc1)
     /-10./(2.*dy**2.)*(sy(ic2+2,jc1)-8.*sy(ic2+1,jc1)+7.*sy(ic2,jc1))
	w(ic2,jc2)=w(ic2,jc2)
     /-10./(2.*dy**2.)*(sy(ic2+2,jc2)-8.*sy(ic2+1,jc2)+7.*sy(ic2,jc2))

c ============= RE START ====== RE START ========
	if (famoor.le.50)goto 100
      do i = 1,n+1
     	do j = 1,n+1

	if (abs(sy(i,j)).ge.1e-9) then
      sydif=Abs((sy(i,j)-sp(i,j))/sy(i,j))
	else
	sydif=0.
	end if
	if (abs(w(i,j)).ge..1e-9)then
	wdif=Abs((w(i,j)-wp(i,j))/w(i,j))
	else
	wdif=0.
	end if
	if (abs(te(i,j)).ge..1e-9)then
	tedif=Abs((te(i,j)-tp(i,j))/te(i,j))
	else
	tedif=0.
	end if

	ii=i
	jj=j
	
	If (sydif-10.**(-qs) .gt.0.0 )goto 100
	If (wdif-10.**(-qw) .gt.0.0)goto 100
	If (tedif-10.**(-qt) .gt.0.0)goto 100

6	end do
	end do

      GoTo 77

c      '=========  Repalace Pervious =========================
100	l =l

      do i = 1,n+1
	do j = 1,n+1
	sp(i,j)=sy(i,j)
	wp(i,j)=w(i,j)
	tp(i,j)=te(i,j)
7	end do
	end do

	GoTo 1000
c     '========= ENTROPY GENERATION =========================
c     '========= ENTROPY GENERATION =========================
77    l = l

      do i = 2,n
 	do j = 2,n

	ty=(te(i,j+1)-te(i,j-1))/(2.*dy)
	tx=(te(i+1,j)-te(i-1,j))/(2.*dy)
	ux=(u(i+1,j)-u(i-1,j))/(2.*dy)
	vy=(v(i,j+1)-v(i,j-1))/(2.*dy)
	uy=(u(i,j+1)-u(i,j-1))/(2.*dy)
	vx=(v(i+1,j)-v(i-1,j))/(2.*dy)
           
	hti(i,j)=tx**2+ty**2
      ffi(i,j)=(2.*(ux**2+vy**2)+(uy+vx)**2)

 	end do
	end do

 	i = 1
  	do j = 2,n
 	ty=(te(i,j+1)-te(i,j-1))/(2.*dy)
	tx=(te(i+1,j)-te(i,j))/(dy)
	ux=(u(i+1,j)-u(i,j))/(dy)
	vy=(v(i,j+1)-v(i,j-1))/(2.*dy)
	uy=(u(i,j+1)-u(i,j-1))/(2.*dy)
	vx=(v(i+1,j)-v(i,j))/(dy)
	hti(i,j)=tx**2+ty**2
      ffi(i,j)=(2.*(ux**2+vy**2)+(uy+vx)**2)
 	end do

      i = n+1
      do j = 2,n
	ty=(te(i,j+1)-te(i,j-1))/(2.*dy)
	tx=(te(i,j)-te(i-1,j))/(dy)
	ux=(u(i,j)-u(i-1,j))/(dy)
	vy=(v(i,j+1)-v(i,j-1))/(2.*dy)
	uy=(u(i,j+1)-u(i,j-1))/(2.*dy)
	vx=(v(i,j)-v(i-1,j))/(dy)
	hti(i,j)=tx**2+ty**2
      ffi(i,j)=(2.*(ux**2+vy**2)+(uy+vx)**2)
 	end do
	
      j=1
	do i = 2,n
	ty=(te(i,j+1)-te(i,j))/(dy)
	tx=(te(i+1,j)-te(i-1,j))/(2.*dy)
	ux=(u(i+1,j)-u(i-1,j))/(2.*dy)
	vy=(v(i,j+1)-v(i,j))/(dy)
	uy=(u(i,j+1)-u(i,j))/(dy)
	vx=(v(i+1,j)-v(i-1,j))/(2.*dy)
	hti(i,j)=tx**2+ty**2
      ffi(i,j)=(2.*(ux**2+vy**2)+(uy+vx)**2)
 	end do


	j=n+1
      do i = 2,n
 	ty=(te(i,j)-te(i,j-1))/(dy)
	tx=(te(i+1,j)-te(i-1,j))/(2.*dy)
	ux=(u(i+1,j)-u(i-1,j))/(2.*dy)
	vy=(v(i,j)-v(i,j-1))/(dy)
	uy=(u(i,j)-u(i,j-1))/(dy)
	vx=(v(i+1,j)-v(i-1,j))/(2.*dy)
	hti(i,j)=tx**2+ty**2
      ffi(i,j)=(2.*(ux**2+vy**2)+(uy+vx)**2)
 	end do

	do i=ic1,ic2
	do j=jc1,jc2
	hti(i,j)=0.
      ffi(i,j)=0.
	be1(i,j)=1.
	end do
	end do

	ffi(1,1)=(ffi(1+1,1)+ffi(1,1+1))/2
	ffi(1,n+1)=(ffi(1+1,n+1)+ffi(1,n+1-1))/2
	ffi(n+1,1)=(ffi(n+1-1,1)+ffi(n+1,1+1))/2
	ffi(n+1,n+1)=(ffi(n+1-1,n+1)+ffi(n+1,n+1-1))/2

	hti(1,1)=(hti(1+1,1)+hti(1,1+1))/2
	hti(1,n+1)=(hti(1+1,n+1)+hti(1,n+1-1))/2
	hti(n+1,1)=(hti(n+1-1,1)+hti(n+1,1+1))/2
	hti(n+1,n+1)=(hti(n+1-1,n+1)+hti(n+1,n+1-1))/2
      
	ii=6

	do aomega=.02,.1005,.02
	write(*,*)'**********',aomega
	sumffi=0. 
	sumhti=0.
	sumbe=0.
	sumns=0. 
      
	do i=1,n+1
	do j=1,n+1
	ht1(i,j)=hti(i,j)/(1.+aomega*te(i,j))**2
	ff1(i,j)=ffi(i,j)*ge/(ra*aomega*(1+aomega*te(i,j)))
	ans1(i,j)=(ff1(i,j)+ht1(i,j))

	If ((i .ge. ic1) .And. (i .le. ic2)) Then
	If ((j .ge. jc1) .And. (j .le. jc2)) Then
	GoTo 11
	End If
	End If
	be1(i,j)=ht1(i,j)/(ff1(i,j)+ht1(i,j))
	
11	sumffi=sumffi+ff1(i,j)*(dy*dy)
	sumhti=sumhti+ht1(i,j)*(dy*dy)
	sumbe=sumbe+be1(i,j)*(dy*dy)

	if((i.eq.1).or.(i.eq.n+1).or.(j.eq.1).or.(j.eq.n+1))then
      sumffi=sumffi-ff1(i,j)*(dy*dy)/2.
	sumhti=sumhti-ht1(i,j)*(dy*dy)/2.
	sumbe=sumbe-be1(i,j)*(dy*dy)/2.	
      end if

      end do
	end do
   	
      sumns=sumhti+sumffi
     	ii=ii-1
	FFIbar(ii)=sumffi
	HTIbar(ii)=sumhti
	BEbar(ii)=sumbe-(be1(1,1)+be1(1,n+1)+be1(n+1,1)+be1(n+1,n+1))
     /*dy*dy/4.
	Sbar(ii)=sumns

	if (ii.eq.5)then
	ff9=ff1
	ht9=ht1
	be9=be1
	ans9=ans1
	end if
	if (ii.eq.4)then
 	ff7=ff1
	ht7=ht1
	be7=be1
	ans7=ans1
	end if
	if (ii.eq.3)then
 	ff5=ff1
 	ht5=ht1
	be5=be1
	ans5=ans1
	end if
	if (ii.eq.2)then
 	ff3=ff1
 	ht3=ht1
	be3=be1
	ans3=ans1
	end if
      if (ii.eq.1)then
 	ff1=ff1
 	ht1=ht1
	be1=be1
	ans1=ans1
	end if

	
	end do

	symax=0.
	ansmax1=0.
	ansmax3=0.
	ansmax5=0.
	ansmax7=0.
	ansmax9=0.
	do i=1,n+1
	do j=1,n+1
	if (symax.lt.abs(sy(i,j))) symax=abs(sy(i,j)) 
	if (ansmax1.lt.abs(ans1(i,j))) ansmax1=abs(ans1(i,j))
	if (ansmax3.lt.abs(ans3(i,j))) ansmax3=abs(ans3(i,j))
	if (ansmax5.lt.abs(ans5(i,j))) ansmax5=abs(ans5(i,j))
	if (ansmax7.lt.abs(ans7(i,j))) ansmax7=abs(ans7(i,j))
	if (ansmax9.lt.abs(ans9(i,j))) ansmax9=abs(ans9(i,j))
      end do
	end do

c===============  PLOT  PLOT =========================	
c===============  PLOT  PLOT =========================
      open(1,file="00Velocity.plt")
	open(2,file="00Stream.plt")
	open(3,file="00Teta.plt")
	open(4,file="00Vorticity.plt")
	open(6,file="Details.txt") 

      open(7,file="1FFI.plt") 
	open(8,file="1HTI.plt")
      open(9,file="1NS.plt")
	open(10,file="1Be.plt")
	
      open(11,file="3FFI.plt") 
	open(12,file="3HTI.plt")
      open(13,file="3NS.plt")
	open(14,file="3Be.plt")
	
      open(15,file="5FFI.plt") 	 
	open(16,file="5HTI.plt")
      open(17,file="5NS.plt")
	open(18,file="5Be.plt")

	open(19,file="7FFI.plt") 	 
	open(20,file="7HTI.plt")
      open(21,file="7NS.plt")
	open(22,file="7Be.plt")

	open(23,file="9FFI.plt") 	 
	open(24,file="9HTI.plt")
      open(25,file="9NS.plt")
	open(26,file="9Be.plt")


	

		

	write(1,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(2,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(3,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(4,*)"zone", "   i=", n + 1, "   j=", n + 1
	
	write(7,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(8,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(9,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(10,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(11,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(12,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(13,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(14,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(15,*)"zone", "   i=", n + 1, "   j=", n + 1
      write(16,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(17,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(18,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(19,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(20,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(21,*)"zone", "   i=", n + 1, "   j=", n + 1
 	write(22,*)"zone", "   i=", n + 1, "   j=", n + 1
 	write(23,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(24,*)"zone", "   i=", n + 1, "   j=", n + 1
	write(25,*)"zone", "   i=", n + 1, "   j=", n + 1
 	write(26,*)"zone", "   i=", n + 1, "   j=", n + 1
	

      do i = 1,n+1
	do j = 1,n+1
	write(1,*) (i-1)*dy,(j-1)*dy,u(i,j),v(i, j)
	write(2,*) (i-1)*dy,(j-1)*dy,sy(i,j)/symax
	write(3,*) (i-1)*dy,(j-1)*dy,te(i,j)
	write(4,*) (i-1)*dy,(j-1)*dy,w(i,j)
	
	write(7,*) (i-1)*dy,(j-1)*dy,ff1(i,j)
	write(8,*) (i-1)*dy,(j-1)*dy,ht1(i,j)
	write(9,*) (i-1)*dy,(j-1)*dy,ans1(i,j)/ansmax1
	write(10,*) (i-1)*dy,(j-1)*dy,be1(i,j)

	write(11,*) (i-1)*dy,(j-1)*dy,ff3(i,j)
 	write(12,*) (i-1)*dy,(j-1)*dy,ht3(i,j)
	write(13,*) (i-1)*dy,(j-1)*dy,ans3(i,j)/ansmax3
	write(14,*) (i-1)*dy,(j-1)*dy,be3(i,j)

	write(15,*) (i-1)*dy,(j-1)*dy,ff5(i,j)
	write(16,*) (i-1)*dy,(j-1)*dy,ht5(i,j)
	write(17,*) (i-1)*dy,(j-1)*dy,ans5(i,j)/ansmax5
	write(18,*) (i-1)*dy,(j-1)*dy,be5(i,j)

	write(19,*) (i-1)*dy,(j-1)*dy,ff7(i,j)
 	write(20,*) (i-1)*dy,(j-1)*dy,ht7(i,j)
	write(21,*) (i-1)*dy,(j-1)*dy,ans7(i,j)/ansmax7
	write(22,*) (i-1)*dy,(j-1)*dy,be7(i,j)

	write(23,*) (i-1)*dy,(j-1)*dy,ff9(i,j)
	write(24,*) (i-1)*dy,(j-1)*dy,ht9(i,j)
	write(25,*) (i-1)*dy,(j-1)*dy,ans9(i,j)/ansmax9
	write(26,*) (i-1)*dy,(j-1)*dy,be9(i,j)
	
8     end do
      end do

	close(1) 
	close(2) 
	close(3) 
	close(4) 
	
	close(7) 
	close(8)
	close(9)
	close(10) 
 	close(11) 
	close(12) 
	close(13) 
	close(14) 
	close(15) 
	close(16)
	close(17)
	close(18) 
 	close(19) 
	close(21) 
	close(22) 
	close(23) 
	close(24) 
	close(25)
	close(26)


	 
c==============  NUSELT  NUSELT  ====================
c==============  NUSELT  NUSELT  ====================
	anu4= 0
	anu3= 0
	anu2= 0	
	anu6=0
	do j = 1, n + 1
	anu4=anu4+abs(-11*te(1,j)+18*te(2,j)-9*te(3,j)+2*te(4,j))/6
	anu3=anu3+abs(-3*te(1,j)+4*te(2,j)-1*te(3,j))/2
	anu2=anu2+abs(-te(1,j)+1*te(2,j))/1
	end do

	do jj = 1, n + 1
c      aaaaaaaaaaaaa	
      do i=1,5
	x=(i-1)*dy  
      do j=1,5
	a(i,j)=x**(j-1.)
	end do
	a(i,6)=te(i,jj) 
      end do

	np=5
      do j=1,np
	do i=1,np
	if (i.eq.j) GoTo 10
	r = -a(i, j) / a(j, j)
	do k=1,np + 1
	a(i, k) = r * a(j, k) + a(i, k)
	end do
	a(i, j)=0
10    end do
      end do
	do i=1,np
	xx(i) = a(i, np + 1) / a(i, i)
	end do	
c        integral
	b1=xx(2)
	anu6=anu6+b1*dy
      end do
c===============  PRINT  ===================      
c===============  PRINT  ===================
	write(6,*)"Grid=",n+1,"*",n+1
	write(6,*)"Itaration=",famoor 
	write(6,*) "====================================="
	write(6,*)"Ra==>>",ra
	write(6,*)"Pr==>>",pr
	write(6,*) "====================================="
	write(6,*)"EROOR==>>"
	write(6,*)"E[w]=",-qw
      write(6,*)"E[s]=",-qs
	write(6,*)"E[t]=",-qt
	write(6,*) "====================================="
	write(6,*)"Nuselt==>>"
	write(6,*)"2Point=",anu2/2
	write(6,*)"3Ppint=",anu3/2
	write(6,*)"4Ppint=",anu4/2
	write(6,*)"CrvFit=",anu6/2
	write(6,*)"SY max=",symax
	write(*,*) "====================================="
	write(6,*)"Enteropy==>>"
	write(6,*)" <FFI>="
	write(6,*)FFIbar(5)
	write(6,*)FFIbar(4)
	write(6,*)FFIbar(3)
	write(6,*)FFIbar(2)
	write(6,*)FFIbar(1)
	write(6,*)" <HTI>="
      write(6,*)HTIbar(5)
	write(6,*)HTIbar(4)
	write(6,*)HTIbar(3)
	write(6,*)HTIbar(2)
	write(6,*)HTIbar(1)
	write(6,*)" <Be >="
	write(6,*)BEbar(5)
	write(6,*)BEbar(4)
	write(6,*)BEbar(3)
	write(6,*)BEbar(2)
	write(6,*)BEbar(1)
	write(6,*)" <S  >="
	write(6,*)Sbar(5)/(1.-.5*.05)
	write(6,*)Sbar(4)/(1.-.5*.05)
 	write(6,*)Sbar(3)/(1.-.5*.05)
 	write(6,*)Sbar(2)/(1.-.5*.05)
	write(6,*)Sbar(1)/(1.-.5*.05)
	write(6,*)" <Ns max  >="
 	write(6,*)ansmax1
	write(6,*)ansmax3
 	write(6,*)ansmax5
 	write(6,*)ansmax7
	write(6,*)ansmax9
	write(6,*) "====================================="

	write(*,*)
	write(*,*)"==========  FINISHED  ================"
      write(*,*)"Grid=",n+1,"*",n+1
	write(*,*)"Itaration=",famoor 
	write(*,*) "====================================="
	write(*,*)"Ra==>>",ra
	write(*,*)"Pr==>>",pr
	write(*,*) "====================================="
	write(*,*)"EROOR==>>"
	write(*,*)"E[t]=",-qt
	write(*,*) "====================================="
	write(*,*)"Nuselt==>>"
	write(*,*)"CrvFit=",anu6

	write(*,*) "====================================="
	write(*,*)"Enteropy==>>"
	write(*,*)" <FFI>="
	write(*,*)FFIbar(5)
	write(*,*)" <HTI>="
   	write(*,*)HTIbar(5)
	write(*,*)" <Be >="
	write(*,*)BEbar(5)
	write(*,*)" <S  >="
	write(*,*)Sbar(5)
	write(*,*) "====================================="
	
	stop
	end
