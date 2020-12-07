
!---------------------------------------------------------------------------
! Programme simulant la diffraction de Fraunhofer par differentes 
! fonctions de transprence.
!---------------------------------------------------------------------------

!---------------------------------------------------------------------------
! Module enregistrant toutes les constantes physiques et les fonctions
! necesaires à simuler les diffractions de Fraunhofer
!---------------------------------------------------------------------------
module Fraunhofer
  implicit none

  !----------------------------------------------------------------------------
  ! Constantes physiques
  !----------------------------------------------------------------------------  
  
  integer, parameter :: n=500
  real,parameter :: PI = 4 * atan(1.0)
  real,parameter :: f  = 30.0e-2      ! distance focale de la lentille en metres
  real,parameter :: lambda = 650.0e-9 ! longueur d'onde en metres
  real,parameter :: k  = 2*PI/(lambda)! nombre d'onde en metres^-1
  real,parameter :: fenetre = 5e-2    ! taille de la fenetre d'observation en metres
  real :: x
  real :: dx = fenetre/n
  real :: y
  real :: dy = fenetre/n
  
contains

  !----------------------------------------------------------------------------
  ! Subroutine FFT en 2D de a
  !----------------------------------------------------------------------------
  subroutine cfft2(a,n)
    implicit none
    integer, intent(in) :: n
    complex, dimension (n,n), intent(inout) :: a
    real, dimension (4*n+15) :: w
    complex, dimension (n) :: s
    integer :: l,m

    call cffti(n,w)
    do l = 1, n
      s = a(:,l) ; call cfftf(n,s,w) ; a(:,l) = cshift(s,n/2)
    enddo
    do m = 1, n
      s = a(m,:) ; call cfftf(n,s,w) ; a(m,:) = cshift(s,n/2)
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
  ! Subroutine crée une fente circulaire en a dans la position (PosX,PosY)
  ! et d'un rayon donné
  !----------------------------------------------------------------------------
  subroutine cercle(a,N,PosX, PosY,rayon)
    implicit none
    integer,intent(in) :: PosX
    integer,intent(in) :: PosY
    integer,intent(in) :: rayon
    integer,intent(in) :: N
    complex,dimension(N,N),intent(inout) :: a
    integer :: l,m
    
    do l = 1, n
      do m = 1,n
        if ((l - PosY )**2 + (m - PosX )**2 < rayon**2) a( l , m )  = (1.,0.)
      enddo
    enddo
    

  end subroutine cercle

  !----------------------------------------------------------------------------
  ! Subroutine un reseau circulaire en a dans la position (PosX,PosY) 
  ! et de cercles de rayon donné
  !----------------------------------------------------------------------------
  subroutine ReseauRond(a,N,PosX, PosY,rayon,Dist)
    implicit none
    integer,intent(in) :: PosX
    integer,intent(in) :: PosY
    integer,intent(in) :: rayon
    integer,intent(in) :: N,Dist 
    ! N/Dist est le nombre de fentes presentes dans le reseau et Dist la distance qui les separe
    integer            :: m,j
    complex,dimension(N,N),intent(inout) :: a

    do m = 0, ( int( N/Dist ) - 1)
      do j = 0, ( int( N/Dist ) - 1)
        call cercle(a,N,PosX + m*dist, PosY + j*dist, rayon)
      end do
    end do

  end subroutine ReseauRond


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
    integer            :: m
    complex,dimension(N,N),intent(inout) :: a

    do m = 0, ( int( N/Dist ) - 1)
      call fente(a,N,PosX + m*dist, PosY,Long,Lar)
    end do

  end subroutine reseau

  !----------------------------------------------------------------------------
  ! Subroutine un reseau de carrés en a dans la position (PosX,PosY) 
  ! et de carrées de longueurs et largeurs donnée
  !----------------------------------------------------------------------------
  subroutine RedSQR(a,N,PosX, PosY,Long,Lar,Dist)
    implicit none
    integer,intent(in) :: PosX
    integer,intent(in) :: PosY
    integer,intent(in) :: Long
    integer,intent(in) :: Lar
    integer,intent(in) :: N,Dist 
    ! Num est le nombre de fentes presentes dans le reseau et Dist la distance qui les separe
    integer            :: m,j
    complex,dimension(N,N),intent(inout) :: a

    do m = 0, ( int( N/Dist ) - 1)
      do j = 0, ( int( N/Dist ) - 1)
        call fente(a,N,PosX + m*dist, PosY + j*dist,Long,Lar)
      end do
    end do

  end subroutine RedSQR

  !----------------------------------------------------------------------------
  ! Subroutine données (Enregistre les données dans un nom donné)
  !----------------------------------------------------------------------------
  subroutine data(a, FFT, n, name)
    implicit none
    integer,intent(in)                  :: n
    complex, dimension (n,n),intent(in) :: a
    complex, dimension (n,n),intent(in) :: FFT
    character (len=*), intent(in)   :: name
    integer   :: l,m
    real      :: x,y
    real      :: ji = 1/(lambda*f)
    open(unit=10,file = trim(name) )

    do l=1,n
      x = (l - n/2)

      do m=1,n
          y = (m - n/2)

          write(10,*) x*dx,' ',y*dy,' ',real(a(l,m)),'  ',x*dx,'  ',y*dy,'  ',((ji)**2)*real( FFT(l,m)*conjg( FFT(l,m) ))
          !write(*,*) x,' ',y,' ',real(a(l,m)),'  ',x*dx/(lambda*f),'  ',y*dy/(lambda*f),'  ',(1/(lambda*f)**2)*real( FFT(l,m)*conjg( FFT(l,m)))

      end do
      
      write(10,*)
      
    end do

    close(10)

  end subroutine data

end module Fraunhofer


program g
  use Fraunhofer ! Importation des subroutines et constantes du probleme
  complex, dimension (n,n) :: a,FFT
  integer                  :: nom
  integer                  :: Long,lar,PosX,PosY,Dist,rayon
  character (len=10)       :: file_name

  !----------------------------------------------------------------------------
  ! Construction d'une fentee rectangulaire finie.
  !----------------------------------------------------------------------------
  
  nom      = 1          ! Code de la fente
  a        = (0.,0.)    ! Initialisation à Zero 
  PosX     = 250        ! Les Valeurs prises  par les subroutines sont des entiers
  PosY     = 250        ! Les Valeurs prises  par les subroutines sont des entiers
  Dist     = 20         ! Les Valeurs prises  par les subroutines sont des entiers
  Long     = 0.56e-3/dx ! Les Valeurs prises  par les subroutines sont des entiers
  Lar      = 3*Long     ! Les Valeurs prises  par les subroutines sont des entiers
  
  ! Creation des deux fentes à partir des dimensions données
  call fente (a,N,PosX, PosY,Long,Lar)
  
  ! Calcul et enregistrement des données.
  FFT = a
  call cfft2(FFT,n)

  write (file_name,"('file',i0,'.data')") nom

  call data(a,FFT,N,file_name)


  !----------------------------------------------------------------------------
  ! Construction de deux fentes rectangulaires finies.
  !----------------------------------------------------------------------------
  
  nom      = nom + 1 ! Code de la fente
  a        = (0.,0.) ! Initialisation à Zero 
  PosX     = 250     ! Les Valeurs sont des entiers
  PosY     = 250       ! Les Valeurs sont des entiers
  Dist     = 20      ! Les Valeurs sont des entiers
  Long     = 5       ! Les Valeurs sont des entiers
  Lar      = 20      ! Les Valeurs sont des entiers
  
  ! Creation des deux fentes à partir des dimensions données
  call fente (a,N,PosX, PosY,Long,Lar)        ! Fente Gauche
  call fente (a,N,PosX + Dist, PosY,Long,Lar) ! Fente Droite
  
  ! Calcul et enregistrement des données.
  FFT = a
  call cfft2(FFT,n)
  
  write (file_name,"('file',i0,'.data')") nom

  call data(a,FFT,N,file_name)

  
  !----------------------------------------------------------------------------
  ! Construction de deux fentes rectangulaires infinies.
  !----------------------------------------------------------------------------
  
  nom      = nom + 1 ! Code de la fente
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
  ! Construction d'un reseau de fentes rectangulaires infinies.
  !----------------------------------------------------------------------------
  
  nom      = nom + 1 ! Code du reseau
  a        = (0.,0.) ! Initialisation à Zero 
  PosX     = 250     ! Les Valeurs sont des entiers
  PosY     = 1       ! Les Valeurs sont des entiers
  Dist     = 100     ! Les Valeurs sont des entiers
  Long     = 5       ! Les Valeurs sont des entiers
  Lar      = 499     ! Les Valeurs sont des entiers

  call reseau(a,N,PosX, PosY,Long,Lar,Dist)

  ! Calcul et enregistrement des données.
  FFT = a
  call cfft2(FFT,n)
  
  write (file_name,"('file',i0,'.data')") nom

  call data(a,FFT,N,file_name)

  !----------------------------------------------------------------------------
  ! Construction d'un reseau de fentes rectangulaires finies.
  !----------------------------------------------------------------------------
  
  nom      = nom + 1 ! Code du reseau
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
  
  write (file_name,"('file',i0,'.data')") nom

  call data(a,FFT,N,file_name)
  
  !----------------------------------------------------------------------------
  ! Construction d'une fente circulaire.
  !----------------------------------------------------------------------------
  
  nom      = nom + 1 ! Code du reseau
  a        = (0.,0.) ! Initialisation à Zero
  PosX     = 250     ! Les Valeurs sont des entiers
  PosY     = 250     ! Les Valeurs sont des entiers
  Dist     = 100     ! Les Valeurs sont des entiers
  Long     = 5       ! Les Valeurs sont des entiers
  Lar      = 20      ! Les Valeurs sont des entiers
  rayon    = 20       ! Les Valeurs sont des entiers

  call cercle(a,N,PosX, PosY,rayon)

  ! Calcul et enregistrement des données.
  FFT = a
  call cfft2(FFT,n)
  
  write (file_name,"('file',i0,'.data')") nom

  call data(a,FFT,N,file_name)
  
  !----------------------------------------------------------------------------
  ! Construction d'un reseau 2D de fentes circulaires.
  !----------------------------------------------------------------------------
  
  nom      = nom + 1 ! Code du reseau
  a        = (0.,0.) ! Initialisation à Zero
  PosX     = 50      ! Les Valeurs sont des entiers
  PosY     = 50      ! Les Valeurs sont des entiers
  Dist     = 100     ! Les Valeurs sont des entiers
  Long     = 5       ! Les Valeurs sont des entiers
  Lar      = 20      ! Les Valeurs sont des entiers
  rayon    = 5       ! Les Valeurs sont des entiers
  
  call ReseauRond(a,N,PosX, PosY,rayon,dist)

  ! Calcul et enregistrement des données.
  FFT = a
  call cfft2(FFT,n)
  
  write (file_name,"('file',i0,'.data')") nom

  call data(a,FFT,N,file_name)
  
  !----------------------------------------------------------------------------
  ! Construction d'un reseau 2D de fentes rectangulaires.
  !----------------------------------------------------------------------------
  
  nom      = nom + 1   ! Code du reseau
  a        = (0.,0.)   ! Initialisation à Zero
  PosX     = 20        ! Les Valeurs sont des entiers
  PosY     = 1         ! Les Valeurs sont des entiers
  Dist     = 50        ! Les Valeurs sont des entiers
  Long     = 0.56e-3/dx! Les Valeurs sont des entiers
  Lar      = 3*Long    ! Les Valeurs sont des entiers

  call RedSQR(a,N,PosX, PosY,Long,Lar,Dist)

  ! Calcul et enregistrement des données.
  FFT = a
  call cfft2(FFT,n)
  
  write (file_name,"('file',i0,'.data')") nom

  call data(a,FFT,N,file_name)
  
  

end program g
