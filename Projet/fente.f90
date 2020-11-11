! Programme simulant la diffraction par deux fentes rectangulaire proches.
!---------------------------------------------------------------------------
program g
  implicit none
  integer, parameter :: n=1000
  complex, dimension (n,n) :: a
  real, dimension (4*n+15) :: w
  integer::l,k
  real:: x,y

  a=(0.,0.) 

  !----------------------------------------------------------------------------
  ! Construction de deux fentes rectangulaire.
  !----------------------------------------------------------------------------

  a(n/2-20:n/2-10, n/2-10:n/2+10)=(1.,0.)
  a(n/2+10:n/2+20, n/2-10:n/2+10)=(1.,0.)

  open(12,file='fente.data')
  do l = 1, n
    do k = 1, n

      write(12,*) l, k, real(a(l,k))

    enddo
  enddo

  close(12)

  !----------------------------------------------------------------------------
  ! Affichage des deux fentes.
  !----------------------------------------------------------------------------

  call cfft2(a,n)

  open(unit=10,file='fraunhofer.data')
  do l=1,n
     x=10*dble(l-n/2)/n

     do k=1,n
        y=10*dble(k-n/2)/n

        write(10,*)x,y,real(a(l,k)*conjg(a(l,k)))
    end do

    write(10,*)
    
  end do 

end program g

  !----------------------------------------------------------------------------
  ! Subroutine FFT en 2D de a
  !----------------------------------------------------------------------------
subroutine cfft2(a,n)
  implicit none
  integer, intent(in) :: n
  complex, dimension (n,n), intent(inout) :: a
  real, dimension (4*n+15) :: w
  complex, dimension (n) :: s
  integer :: l,k

  call dffti(n,w)
  do l = 1, n
    s = a(:,l) ; call dfftf(n,s,w) ; a(:,l) = cshift(s,n/2)
  enddo
  do k = 1, n
    s = a(k,:) ; call dfftf(n,s,w) ; a(k,:) = cshift(s,n/2)
  enddo

end subroutine cfft2
