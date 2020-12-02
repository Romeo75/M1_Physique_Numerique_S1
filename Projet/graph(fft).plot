#FFT de la fente
#i=5
#  set terminal png
#  set output 'fente_'.i.'_FFT.png'

set view map
set pm3d at b
set palette color positive

set ylabel ' y( entier sans unité ) '
set xlabel ' x( entier sans unité )'
set title 'FFT de la fente'.i
set autoscale
if ( i == 1 ){
        splot [*:*][-8:10] 'file'.i.'.data' u 1:2:4 with points palette pt 5 ps 0.5 ##lol##
        replot
        } else {
            if (i == 2){
                splot [*:*][-8:10] 'file'.i.'.data' u 1:2:4 with points palette pt 5 ps 0.5 ##lol##
                replot
            } else{
                 if (i == 3){
                    splot [*:*][-150:150] 'file'.i.'.data' u 1:2:4 with points palette pt 5 ps 0.5 ##lol##
                    replot
                } else{
                
                     if (i == 4){
                        splot [-50:50] [-50:50] 'file'.i.'.data' u 1:2:4 with points palette pt 5 ps 0.5 ##lol##
                        replot
                    } else {
                         if (i == 5){
                            splot [-100:100][-100:100] 'file'.i.'.data' u 1:2:4 with points palette pt 5 ps 0.5 ##lol##
                            replot
                        } else{
                            if (i == 6){
                            splot [*:*][*:*] 'file'.i.'.data' u 1:2:4 with points palette pt 5 ps 0.5 ##lol##
                            replot
                            } else {
                                splot 'file'.i.'.data' u 1:2:4 with points palette pt 5 ps 0.5 ##lol##
                                replot
                            }
                        }
                    }
                }
            }
        }

replot

#   set terminal x11