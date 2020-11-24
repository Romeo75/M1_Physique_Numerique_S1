#FFT de la fente
 i=5
#  set terminal png
#  set output 'fente_'.i.'_FFT.png'

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

#   set terminal x11