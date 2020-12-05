N = 8 #nombre de fichiers *.data

set terminal png
set samples 50; set isosamples 50


do for [i = 1:N] {
    
    if (i>1) {clear}
    set output 'fente_'.i.'.png'

    #Fente
    set pm3d at b
    set palette color positive
    set view map
    
    set xlabel ' x( en cm) '
    set ylabel ' y( en cm) '
    set title 'Fonction de transparence de la fente'.i
    set autoscale

    if ( i == 1 ){
            splot [-0.2:0.3][-0.2:0.3][*:*] 'file'.i.'.data' u (100*$1):(100*$2):3 with points pointtype 5 pointsize 1.0 palette linewidth 0.01
            replot
            } else {

    if ( i == 2 ){
            splot [*:*][*:*][*:*] 'file'.i.'.data' u (100*$1):(100*$2):3 with points pointtype 5 pointsize 1.0 palette linewidth 0.01
            replot
            } else {

    if ( i == 3 ){
            splot [*:*][*:*][*:*] 'file'.i.'.data' u (100*$1):(100*$2):3 with points pointtype 5 pointsize 1.0 palette linewidth 0.01
            replot
            } else {
                if (i == 4){
                    splot [*:*][*:*][*:*] 'file'.i.'.data' u (100*$1):(100*$2):3 with points pointtype 5 pointsize 1.0 palette linewidth 0.01
                    replot
                } else{
                    if (i == 5){
                        splot [*:*][*:*][*:*] 'file'.i.'.data' u (100*$1):(100*$2):3 with points pointtype 5 pointsize 1.0 palette linewidth 0.01
                        replot
                    } else{
                    
                        if (i == 6){
                            splot [*:*][*:*][*:*] 'file'.i.'.data' u (100*$1):(100*$2):3 with points pointtype 5 pointsize 1.0 palette linewidth 0.01
                            replot
                        } else {
                            if (i == 7){
                                splot [*:*][*:*][*:*] 'file'.i.'.data' u (100*$1):(100*$2):3 with points pointtype 5 pointsize 1.0 palette linewidth 0.01
                                replot
                            } else{
                                if (i == 8){
                                splot [*:*][*:*][*:*] 'file'.i.'.data' u (100*$1):(100*$2):3 with points pointtype 5 pointsize 1.0 palette linewidth 0.01
                                replot
                                } else {
                                    splot 'file'.i.'.data' u (100*$1):(100*$2):3 with points pointtype 5 pointsize 1.0 palette linewidth 0.01
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
