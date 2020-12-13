
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
    subroutine ReseauRond(a,N,PosX, PosY,Dist,rayon)
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
  

program anime
    use Fraunhofer ! Importation des subroutines et constantes du probleme
    complex, dimension (n,n) :: a,FFT
    integer                  :: nom
    integer                  :: PosX,PosY,Dist,rayon
    character (len=15)       :: file_name
    integer                  :: anim,number = 10
    
  !----------------------------------------------------------------------------
  ! Construction d'un reseau 2D de fentes circulaires.
  !----------------------------------------------------------------------------
  
  nom      = nom + 1    ! Code du reseau
  a        = (0.,0.)    ! Initialisation à Zero
  Dist     = 0.1e-2/dx  ! Les Valeurs sont des entiers
  rayon    = 0.5e-3/dx  ! Les Valeurs sont des entiers
  PosX     = rayon      ! Les Valeurs sont des entiers
  PosY     = rayon      ! Les Valeurs sont des entiers
  
  
  do anim = 1, number
    a        = (0.,0.)
    write(*,*) 'dist: ',dist,'  ','rayon: ',rayon
    
    call ReseauRond(a,N,PosX, PosY,dist,rayon)
  
    ! Calcul et enregistrement des données.
    FFT = a
    call cfft2(FFT,n)
    write (file_name,"('anim',i0,'.data')") nom
  
    call data(a,FFT,N,file_name)
    
    nom     = nom   + 1
    !rayon    = rayon  + 0.1e-2/dx
    dist    = number*0.1e-2/dx

  end do

end program anime