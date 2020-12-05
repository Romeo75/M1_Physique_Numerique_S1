N = 8 #nombre de fichiers *.data

set terminal png

do for [i = 1:N] {

    if (i>1) {clear}
    set output 'fente_'.i.'_FFT.png'

    #Fente
    set pm3d at b
    set palette color positive
    set view map

    set xlabel " x'( en mm ) "    
    set ylabel " y'( en mm ) "
    set title 'FFT de la fente'.i
    set autoscale

    if ( i == 1 ){
            splot [-2:2][-2:2] 'file'.i.'.data' u ($4*1000):($5*1000):6 with points palette pt 5 ps 0.01 linewidth 0.01
            replot
            } else {

    if ( i == 2 ){
            splot [*:*][*:*] 'file'.i.'.data' u ($4*1000):($5*1000):6 with points palette pt 5 ps 0.01 linewidth 0.01
            replot
            } else {

    if ( i == 3 ){
            splot [*:*][-2:2] 'file'.i.'.data' u ($4*1000):($5*1000):6 with points palette pt 5 ps 0.01 linewidth 0.01
            replot
            } else {
                if (i == 4){
                    splot [*:*][-2:2] 'file'.i.'.data' u ($4*1000):($5*1000):6 with points palette pt 5 ps 0.01 linewidth 0.01
                    replot
                } else{
                    if (i == 5){
                        splot [*:*][*:*] 'file'.i.'.data' u ($4*1000):($5*1000):6 with points palette pt 5 ps 0.01 linewidth 0.01
                        replot
                    } else{
                    
                        if (i == 6){
                            splot [*:*][*:*]'file'.i.'.data' u ($4*1000):($5*1000):6 with points palette pt 5 ps 0.01 linewidth 0.01
                            replot
                        } else {
                            if (i == 7 ){
                                splot [*:*][*:*] 'file'.i.'.data' u ($4*1000):($5*1000):6 with points palette pt 5 ps 0.01 linewidth 0.01
                                replot
                            } else{
                                if (i == 8){
                                splot [*:*][*:*]'file'.i.'.data' u ($4*1000):($5*1000):6 with points palette pt 5 ps 0.01 linewidth 0.01
                                replot
                                } else {
                                    splot 'file'.i.'.data' u ($4*1000):($5*1000):6 with points palette pt 5 ps 0.01 linewidth 0.01
                                    replot
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
}

reset