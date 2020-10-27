gam = 1.0
tho = 0.5
Vf = 0.1
Vc = Vf*log( ( gam*tho)/(Vf) )

docs = 15

set terminal png

do for [i = 1:docs] { 
    
    set output 'phase_V0_'.i.'.png'

    plot 'x(t)_'.i.'.dat' u ($2 - $4):3
    V0 = 0.1*i*Vc
    set xlabel ' x(t)_'.i.'-v0*t ( en USI)'
    set ylabel ' Vx(t)_'.i.' ( en USI)'
    set title 'Portrait de phase du pendule libre avec frotement et vitesse V0_'.i.' = '
    replot

}
do for [m = 1:docs] { 
    
    set output 'vitesse_V0_'.m.'.png'

    V0 = 0.1*i*Vc
    plot 'x(t)_'.i.'.dat' u 1:2
    set xlabel ' t ( en USI)'
    set ylabel ' Vx(t)_'.m.' ( en USI)'
    set title 'Vitesse du pendule soumis à un frotement et tiré avec une vitesse v0_'.i.' = '
    replot

}


set terminal x11