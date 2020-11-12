! Programme simulant la diffraction par deux fentes rectangulaire proches.
!---------------------------------------------------------------------------
program g
  implicit none
  integer, parameter :: n=500
  complex, dimension (n,n) :: a,FFT
  real, dimension (4*n+15) :: w
  integer::l,k
  real:: x,y
  integer Long,lar,PosX,PosY,Dist
  
  a        = (0.,0.) ! Initialisation à Zero 
  PosX     = 250 ! Les Valeurs sont des entiers
  PosY     = 1   ! Les Valeurs sont des entiers
  Dist     = 20  ! Les Valeurs sont des entiers
  Long     = 5  ! Les Valeurs sont des entiers
  Lar      = 499 ! Les Valeurs sont des entiers
  
  !----------------------------------------------------------------------------
  ! Construction de deux fentes rectangulaire.
  !----------------------------------------------------------------------------
  
  !Deux fentes des dimensions données
  call fente (a,N,PosX, PosY,Long,Lar)        !Gauche
  call fente (a,N,PosX + Dist, PosY,Long,Lar) !Droite

  !call reseau(a,N,PosX, PosY,Long,Lar,Dist)

  !----------------------------------------------------------------------------
  ! Affichage des deux fentes.
  !----------------------------------------------------------------------------
  
  FFT = a
  call cfft2(FFT,n)

  open(unit=10,file='fente.data')
  do l=1,n
     x=10*dble(l-n/2)/n

     do k=1,n
        y=10*dble(k-n/2)/n

        write(10,*)x,y,real(a(l,k)),real( FFT(l,k)*conjg( FFT(l,k)))
        write(*,*) x,y,real(a(l,k)),real( FFT(l,k)*conjg( FFT(l,k)))
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

  call cffti(n,w)
  do l = 1, n
    s = a(:,l) ; call cfftf(n,s,w) ; a(:,l) = cshift(s,n/2)
  enddo
  do k = 1, n
    s = a(k,:) ; call cfftf(n,s,w) ; a(k,:) = cshift(s,n/2)
  enddo

end subroutine cfft2

!-------------------------------------------------------------------------------------
! Subroutine fente en a dans la position (PosX,PosY) et de longueur et largeur donnée
!-------------------------------------------------------------------------------------
subroutine fente(a,N,PosX, PosY,Long,Lar)
  implicit none
  integer,intent(in) :: PosX
  integer,intent(in) :: PosY
  integer,intent(in) :: Long
  integer,intent(in) :: Lar
  integer,intent(in) :: N
  complex,dimension(N,N),intent(inout) :: a

  a( PosX:PosX + Long , PosY:PosY + Lar )  = (1.,0.)

end subroutine fente

!-------------------------------------------------------------------------------------
! Subroutine fente en a dans la position (PosX,PosY) et de longueur et largeur donnée
!-------------------------------------------------------------------------------------
subroutine reseau(a,N,PosX, PosY,Long,Lar,Dist)
implicit none
integer,intent(in) :: PosX
  integer,intent(in) :: PosY
  integer,intent(in) :: Long
  integer,intent(in) :: Lar
  integer,intent(in) :: N,Dist 
  ! Num est le nombre de fentes presentes dans le reseau et Dist la distance qui les separe
  integer            :: i
  complex,dimension(N,N),intent(inout) :: a

  do i = 0, ( int( N/Dist ) - 1)
    call fente(a,N,PosX + i*dist, PosY,Long,Lar)
  end do

end subroutine reseau