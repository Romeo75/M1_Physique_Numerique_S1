!soubroutine ok
subroutine cfft2(s,n)
implicit none
integer, intent(in) :: n
complex, dimension(n,n), intent(inout) :: s
real, dimension(4*n+15) :: w
integer :: k
call dffti(n,w)
do k = 1, n ; call dfftf(n,s(:,k),w) ; s(:,k) = cshift(s(:,k),n/2)   ; enddo
do k = 1, n ; call dfftf(n,s(k,:),w) ; s(k,:) = cshift(s(k,:),n/2)/n ; enddo
end subroutine cfft2



!program principal

program fraunh
implicit none
integer, parameter :: n=64
complex, dimension(n,n) :: c  
complex :: i2pi=(0.,1._8)*2*acos(-1.0_8)    !intensite 
real, dimension(n,n) :: intens,a,b
real :: x,y,Xs,Ys          !qx,qy
real :: pi=acos(-1.)
real :: x0,y0
real , parameter :: lamda=500.
real , parameter :: d=450.
real , parameter :: psi=1.,k=0.0125
integer :: e,j
complex :: i
!Xs0=3
!Ys0=4
a=(0.,0.) ; b=(0.,0.)
i=(0.,1.)
x0=2.5
y0=2.5

open (1, file='dxxyy')
do e=1,n
x=e*d
  do j=1,n

y=j*d

Xs=e*2*pi/d
  
Ys=j*2*pi/d

 c(e,j)= (1/lamda*lamda*d*d) * exp(i*k*(x*Xs+y*Ys)/d)

write (1,*) x,y,real(c(e,j))
end do
end do

call cfft2(c,n)


intens=c*conjg(c)

   end                                                         !open(2,file='iqxqy')
                                                                    !do i=1,n
                                                              !qx=i*2*pi/a
                                                              ! do j=1,n


                                                         !qy=j*2*pi/a
                                                ! write (2,*) qx,qy,intens(i,j)

                                                   ! end do
                                                !end do


