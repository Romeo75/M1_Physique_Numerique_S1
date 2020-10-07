module tp1
    implicit none

   real, parameter :: gam = 1.0   
   real, parameter :: w0 = 1.0
   real, parameter :: T = 1.0
   real, parameter :: x0 = 0.1  ! Dans ce programme on ecrit l'angle comme la variable 'x' au lieu de la variable 'teta'
   real, parameter :: eps = 1e-7 ! Niveau de precision avec laquelle on veut aproicher le zero de la fonction considérée
    
end
!-------------------------------------------------------


subroutine f(x,y,dy)
    implicit none
    real x0,T,w0,gam
    real x,y,dy
    
    y = ( sin(x)/x )*( x0**2 - x**2 ) - (gam*T)/(w0**2)
    
    !dy ?
    
end
!-------------------------------------------------------

subroutine sini(x,y,dy)
    implicit none
    real x,y,dy
    
    y  = sin(x) 

    dy = cos(x)

end
!-------------------------------------------------------

subroutine e(x,y,dy)
    implicit none
    real x,y,dy
    
    y  = exp(x)

    dy = exp(x)

end
!-------------------------------------------------------


real function newton(f,x0,eps)

    implicit none
    
    real x,y,dy,eps
    real x0
    integer i, imax
    logical convergence
    
    imax = 10
    x = x0
    y = 1.0

    print '(A)'
    do i = 1, imax 
    !La Variable imax permet d'eviter la consomation de ressources trop importante lorsque l'algorithme ne trouve pas de solutions

        !Debug
        write(*,*) 'i = ',i,'    y = ',y, ' x = ',x

        call f(x,y,dy) ! Initialise les valeurs à la ièmme boucle
        
        x = x - y/dy
        
        convergence  = abs(y) < eps
        !La Variable convergence est une variable logique (ou booléenne) qui est vrai lorsque la condition de convergence est verifiée

        if ( convergence .or. i==imax ) then
            
            newton = x
            write(*,*) ' Le zero est à x = ', x
            
            exit 
        endif
    enddo

end
!-------------------------------------------------------

program transphase
    !implicit none

    !Imports
    use tp1
    real :: newton
    external :: f
    external :: sini
    external :: e
    
    !Variables disponibles
    write (*,*) ' Les parametres du probleme sont:    ', 'gam = ', gam, 'w0 = ', w0, 'T = ', T, 'x0 = ', x0, 'eps = ', eps
    
    !On peut disposer des subroutines:
    !   f(x)    [Probleme de la bille]
    !   sini(x)  [fonction test]
    !   e(x)    [fonction test]
    
    !test de sini(x)
    write(*,*) '','test de sini(x)', newton(sini,X0, eps)
    

    
    !test de e(x) = exp(x)
    write(*,*) '','test de e(x)', newton(e,X0,eps)

end
!-------------------------------------------------------