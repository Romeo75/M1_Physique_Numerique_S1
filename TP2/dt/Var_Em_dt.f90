module tp2
    implicit none
    
    !Variables Universelles
    real,   parameter ::  dure = 100
    real,   parameter ::  pas_t = 0.01
    real,   parameter ::  pas_dt = 0.01
    real,   parameter ::  dt_i = 1e-5
    real,   parameter ::  dt_f = 1
    integer,   parameter ::  dim = 2
    
    !Variables du probleme
    real, parameter :: m = 1.0
    real, parameter :: k = 1.0
    real, parameter :: w0 = 1.0
    real, parameter :: x0 = 1.0 
    real, parameter :: v0 = 0.0

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
    
    dx(1) = x(2)
    dx(2) = w0*(v0*t-x(1))

end subroutine deriv



program eqdif
    !implicit none

    use tp2
    external :: deriv
    
    integer                      :: i,q
    integer                      :: n
    real                         :: t,dt
    real                         :: Em,Ec,Ep,Moy_Em,Moy_Em2,Var_dt
    real    ,   dimension(2)     :: x

    !Initialisation des variables
    x(1)= x0
    x(2)= v0
    
    t = 0.0
    dt = pas_t
    n = dim
    Em = 0
    Ec = 0
    Ep = 0
    Moy_Em = 0
    Moy_Em2 = 0
    
    open (1,file = 'x(t).dat')
    open (2,file = 'E(t).dat')
    
    dt = dt_i
    do q = 0, int( abs(dt_i - dt_f) / pas_dt)
        
        do i = 1, int(dure/dt)
    
            call rk4(t,x,dt,n,deriv)
            
            Ec = 0.5 * m * (x(2)**2)
            Ep = 0.5 * k * (x(1)**2)

            Em = Ec + Ep

            Moy_Em = Moy_Em + Em - ( 0.5 * k * (x0**2) + 0.5 * m * (v0**2))

            Moy_Em2 = Moy_Em2 + Em**2 - (0.5 * k * (x0**2) + 0.5 * m * (v0**2))**2

            !Enregistrement
            !write(*,*) '    t = ', t, ' x = ', x(1), '  v = ',x(2),'  Ec = ',Ec,'  Ep = ',Ep,'  Em = ',Em
            !write(1,*) t,'  ',x(1),'   ',x(2)

            t = t + dt
        end do

        Moy_Em = Moy_Em/i
        Moy_Em2 = Moy_Em2/i

        Var_dt = Moy_Em2 - (Moy_Em)**2
        
        write(*,*) '    i = ',i,'   dt = ', dt,' Var_dt = ',Var_dt,' Moy_Em = ',Moy_Em,' Moy_Em2 = ',Moy_Em2
        write(2,*) dt,' ',Var_dt,' ',Moy_Em
        
        Moy_Em = 0
        Moy_Em2 = 0
        

        dt = dt + pas_dt
    end do
    
    close(1)
    close(2)

end program eqdif

