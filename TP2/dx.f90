module tp2
    implicit none
    
    !Variables Universelles
    real,   parameter ::  g = 9.81
    real,   parameter ::  l = 0.1
    real,   parameter ::  m = 6e-3
    
    !Variables du probleme
    real, parameter :: w0 = 1.0
    real, parameter :: x0 = 1.0 
    real, parameter :: v0 = 0.0

    contains
    !Subroutines
        subroutine rk4(t,x,dt,n,deriv)
        !4th order Runge-Kutta
        
            implicit none
        
            integer ,                   intent(in)      :: n
            real    ,                   intent(in)      :: t,dt
            real    ,   dimension(n),   intent(inout)   :: x
            real                                        :: ddt  
            real    ,   dimension(n)                    :: xp, k1, k2, k3, k4   
        
            ddt = 0.5*dt
            call deriv(t,x,k1,n)        ;   xp = x + ddt*k1
            call deriv(t+ddt,xp,k2,n)   ;   xp = x + ddt*k2
            call deriv(t+ddt,xp,k3,n)   ;   xp = x +  dt*k3
            call deriv(t+dt,xp,k4,n)    ;   x  = x +  dt*( k1 + 2.0*k2 + 2.0*k3 + k4 )/6.0
        
            end subroutine rk4
    
end module tp2


subroutine deriv(t,x,dt,n)
    !Système differentiel
    use tp2
    !implicit none

    integer ,                   intent(in)      :: n
    real    ,                   intent(in)      :: t,dt
    real    ,   dimension(n),    intent(inout)   :: x
    real    ,   dimension(n)                    :: dx
    
    dx(1) = x(2)
    dx(2) = w0*(v0*t-x(1))
    end subroutine deriv



program eqdif
    !implicit none

    use tp2
    external :: deriv
    
    integer                      :: i
    integer                      :: n
    real                         :: t,dt
    real    ,   dimension(2)     :: x
    x(1)= x0
    x(2)= v0
    
    
    open (1,file = 'x(t).dat')

    do i = 1, 100

        call rk4(t,dt,n,deriv)
        write(*,*) '    t = ', t, ' x = ', x(1), '  v = ',x(2)
        write(1,*) t,'  ',x(1),'   ',x(2)

    end do
    close(1)

end program eqdif

