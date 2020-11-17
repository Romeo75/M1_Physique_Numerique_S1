! Programme simulant la diffraction par deux fentes rectangulaire proches.
!---------------------------------------------------------------------------
program g
  implicit none
  integer, parameter :: n=500
  complex, dimension (n,n) :: a,FFT
  integer                  :: nom
  integer                  :: Long,lar,PosX,PosY,Dist
  character (len=10)       :: file_name
  
  !----------------------------------------------------------------------------
  ! Construction de deux fentes rectangulaire.
  !----------------------------------------------------------------------------
  
  nom      = 1
  a        = (0.,0.) ! Initialisation à Zero 
  PosX     = 250     ! Les Valeurs sont des entiers
  PosY     = 1       ! Les Valeurs sont des entiers
  Dist     = 20      ! Les Valeurs sont des entiers
  Long     = 5       ! Les Valeurs sont des entiers
  Lar      = 499     ! Les Valeurs sont des entiers
  
  ! Creation des deux fentes à partir des dimensions données
  call fente (a,N,PosX, PosY,Long,Lar)        ! Fente Gauche
  call fente (a,N,PosX + Dist, PosY,Long,Lar) ! Fente Droite
  
  ! Calcul et enregistrement des données.
  FFT = a
  call cfft2(FFT,n)
  
  write (file_name,"('file',i0,'.data')") nom

  call data(a,FFT,N,file_name)


  !----------------------------------------------------------------------------
  ! Construction de 'un reseau rectangulaire infini.
  !----------------------------------------------------------------------------
  
  nom      = 2       ! Code du reseau
  a        = (0.,0.) ! Initialisation à Zero 
  PosX     = 250     ! Les Valeurs sont des entiers
  PosY     = 1       ! Les Valeurs sont des entiers
  Dist     = 100      ! Les Valeurs sont des entiers
  Long     = 5       ! Les Valeurs sont des entiers
  Lar      = 499     ! Les Valeurs sont des entiers

  call reseau(a,N,PosX, PosY,Long,Lar,Dist)

  ! Calcul et enregistrement des données.
  FFT = a
  call cfft2(FFT,n)
  nom = 2
  write (file_name,"('file',i0,'.data')") nom

  call data(a,FFT,N,file_name)

  !----------------------------------------------------------------------------
  ! Construction d'un reseau rectangulaire.
  !----------------------------------------------------------------------------
  
  nom      = 3       ! Code du reseau
  a        = (0.,0.) ! Initialisation à Zero
  PosX     = 250     ! Les Valeurs sont des entiers
  PosY     = 225     ! Les Valeurs sont des entiers
  Dist     = 100     ! Les Valeurs sont des entiers
  Long     = 5       ! Les Valeurs sont des entiers
  Lar      = 20      ! Les Valeurs sont des entiers

  call reseau(a,N,PosX, PosY,Long,Lar,Dist)

  ! Calcul et enregistrement des données.
  FFT = a
  call cfft2(FFT,n)
  nom = 3
  write (file_name,"('file',i0,'.data')") nom

  call data(a,FFT,N,file_name)

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

!----------------------------------------------------------------------------
! Subroutine crée une fente en a dans la position (PosX,PosY)
! et de longueur et largeur donnée
!----------------------------------------------------------------------------
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

!----------------------------------------------------------------------------
! Subroutine un reseau en a dans la position (PosX,PosY) 
! et de longueur et largeur donnée
!----------------------------------------------------------------------------
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

!----------------------------------------------------------------------------
! Subroutine données (Enregistre les données dans un nom donné)
!----------------------------------------------------------------------------
subroutine data(a, FFT, n, name)
  implicit none
  integer,intent(in)                  :: n
  complex, dimension (n,n),intent(in) :: a
  complex, dimension (n,n),intent(in) :: FFT
  character (len=*), intent(in)   :: name
  integer   :: l,k
  real      :: x,y
  
  open(unit=10,file = trim(name) )
  
  do l=1,n
     x = l - n/2

     do k=1,n
        y = k - n/2

        write(10,*)x,y,real(a(l,k)),real( FFT(l,k)*conjg( FFT(l,k)))
        !write(*,*) x,y,real(a(l,k)),real( FFT(l,k)*conjg( FFT(l,k)))
    end do

    write(10,*)
    
  end do

  close(10)

end subroutine data