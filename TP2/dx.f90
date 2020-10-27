module tp2
    implicit none
    
    !Variables Universelles
    real,   parameter ::  dure = 100 !Durée de l'oscilation
    real,   parameter ::  pas = 0.01 !pas dt qui convient à la mesure
    integer,   parameter ::  dim = 2 !nombre de variables canoniques du systeme
    
    !Variables du probleme
    real, parameter :: w0 = 1.0 ! Vitesse angulaire caracteristique
    real, parameter :: x0 = 0.0 ! Position initiale
    real, parameter :: v0 = 0.0 ! Vitesse initiale
    real, parameter :: gam = 1.0
    real, parameter :: tho = 0.5
    real, parameter :: Vf = 0.1
    real, parameter :: Vc = Vf*log( ( gam*tho)/(Vf) )
    real            :: v_push = 0.1*Vc ! Vitesse initiale

    contains
    !Subroutines
    
    subroutine rk4(t,x,dt,n,deriv)
    !4th order Runge-Kutta
    
        implicit none
    
        integer           , intent(in)    :: n
        real              , intent(in)    :: t, dt
        real, dimension(n), intent(inout) :: x
        real                              :: ddt
        real, dimension(n)                :: xp, k1, k2, k3, k4
    
        ddt = 0.5*dt
    
        call deriv(t,x,k1,n)      ; xp = x + ddt*k1
        call deriv(t+ddt,xp,k2,n) ; xp = x + ddt*k2
        call deriv(t+ddt,xp,k3,n) ; xp = x +  dt*k3
        call deriv(t+dt,xp,k4,n)  ; x  = x +  dt*( k1 + 2.0*k2 + 2.0*k3 + k4 )/6.0
        
    end subroutine rk4
    
end module tp2


subroutine deriv(t,x,dx,n)
    !Système differentiel
    use tp2
    !implicit none

    integer ,                   intent(in)      :: n
    real    ,                   intent(in)      :: t
    real    ,   dimension(n),   intent(inout)   :: x
    real    ,   dimension(n)                    :: dx
    real                                        :: V_sign
    
    !V_sign = int( x(2)/abs(x(2)) )


    dx(1) = x(2)
    dx(2) = (w0**2)*(v_push*t-x(1)) - dx(1)/tho - sign(gam,dx(1))*exp( -abs(dx(1))/Vf )

end subroutine deriv



program eqdif
    !implicit none

    use tp2
    external :: deriv
    
    integer                      :: i
    integer                      :: n
    real                         :: t,dt
    real    ,   dimension(2)     :: x
    
    integer :: q
    character(len=10) :: file_id
    character(len=50) :: file_name

    !Initialisation des variables
    x(1)= x0
    x(2)= V0
    
    t = 0.0
    dt = pas
    n = dim
    
    !open (1,file = 'x(t).dat')

    do q = 1, 15
        
        ! Write the integer into a string:
        write(file_id, '(i0)') q
    
        ! Construct the filename:
        file_name = 'x(t)_' // trim(adjustl(file_id)) // '.dat'
    
        ! Open the file with this name
        open(file = trim(file_name), unit = q)
        
        write(*,*) 'V_push = ',V_push
        write(q,*) '#V_push = ',V_push

        do i = 1, int(dure/dt)
    
            call rk4(t,x,dt,n,deriv)
            write(*,*) '    t = ', t, ' x = ', x(1), '  v = ',x(2),'  v_push*t = ',v_push*t
            write(q,*) t,'  ',x(1),'   ',x(2),'	',v_push*t
            t = t + dt
     
        end do

        close(q)

        V_push = q*0.1*Vc

    end do
    

end program eqdif

