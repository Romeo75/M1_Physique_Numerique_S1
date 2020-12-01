N = 3 #nombre de fichiers *.data

set terminal png

do for [i = 1:N] {
    
    set output 'fente_'.i.'.png'

    set view map
    set size ratio .9

    set object 1 rect from graph 0, graph 0 to graph 1, graph 1 back
    set object 1 rect fc rgb "black" fillstyle solid 1.0

    #Fente
    set xlabel ' x( en USI)'
    set ylabel ' y( en USI)'
    set title 'Matrice de la fente'.i
    set autoscale

    
    if ( i == 1 ){
            splot [-10:35][*:*][*:*] 'file'.i.'.data' u 1:2:3 with points pointtype 5 pointsize 1.1 palette linewidth 30
            replot
            } else {
                if (i == 2){
                    splot [*:*][*:*][0:1] 'file'.i.'.data' u 1:2:3 with points pointtype 5 pointsize 0.3 palette linewidth 30
                    replot
                } else{
                     if (i == 3){
                        splot [*:*][*:*][0:1] 'file'.i.'.data' u 1:2:3 with points pointtype 5 pointsize 1 palette linewidth 30
                        replot
                    } else{
                    
                         if (i == 4){
                            splot [-60:60][-60:60][0:1] 'file'.i.'.data' u 1:2:3 with points pointtype 5 pointsize 1 palette linewidth 30
                            replot
                        } else {
                             if (i == 5){
                                splot [*:*][*:*][0:1] 'file'.i.'.data' u 1:2:3 with points pointtype 5 pointsize 0.5 palette linewidth 30
                                replot
                            } else{
                                if (i == 6){
                                splot [*:*][*:*][0:1] 'file'.i.'.data' u 1:2:3 with points pointtype 5 pointsize 1 palette linewidth 30
                                replot
                                } else {
                                    splot 'file'.i.'.data' u 1:2:3 with points pointtype 5 pointsize 1 palette linewidth 30
                                    replot
                                }
                            }
                        }
                    }
                }
            }

clear

}

reset

set terminal png

do for [i = 1:N] {

    set output 'fente_'.i.'_FFT.png'

    #Fente
    set pm3d map
    set xlabel ' x( en USI)'
    set ylabel ' y( en USI)'
    set title 'FFT de la fente'.i
    set autoscale

    if ( i == 1 ){
        splot [*:*][-8:10] 'file'.i.'.data' u 1:2:4 with points pointtype 5 pointsize 2.9 palette linewidth 30
        replot
        } else {
            if (i == 2){
                splot [*:*][-8:10] 'file'.i.'.data' u 1:2:4 with points pointtype 5 pointsize 2.9 palette linewidth 30
                replot
            } else{
                 if (i == 3){
                    splot [*:*][-150:150] 'file'.i.'.data' u 1:2:4 with points pointtype 5 pointsize 1 palette linewidth 30
                    replot
                } else{
                
                     if (i == 4){
                        splot [-50:50] [-50:50] 'file'.i.'.data' u 1:2:4 with points pointtype 5 pointsize 1 palette linewidth 30
                        replot
                    } else {
                         if (i == 5){
                            splot [*:*][*:*] 'file'.i.'.data' u 1:2:4 with points pointtype 5 pointsize 0.5 palette linewidth 30
                            replot
                        } else{
                            if (i == 6){
                            splot [*:*][*:*] 'file'.i.'.data' u 1:2:4 with points pointtype 5 pointsize 1 palette linewidth 30
                            replot
                            } else {
                                splot 'file'.i.'.data' u 1:2:4 with points pointtype 5 pointsize 1.1 palette linewidth 30
                                replot
                            }
                        }
                    }
                }
            }
        }

    replot
    
    clear
    
}

reset