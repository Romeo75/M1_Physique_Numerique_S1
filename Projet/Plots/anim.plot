#FFT de la fente
N = 10
 set terminal png
#  set output 'fente_'.i.'_FFT.png'

#unset surface
#set samples 50; set isosamples 50
#Fente
do for [i = 1:N:1] {
    
    set output 'fente_anim_'.i.'_FFT.png'

    #Fente
    set pm3d at b
    set palette color positive
    set view map

    set xlabel " x'( en mm ) "    
    set ylabel " y'( en mm ) "
    set title 'FFT de la fente'.i
    set autoscale
    splot [*:*][*:*] 'anim'.i.'.data' u ($4*1000):($5*1000):3 with points palette pt 5 ps 0.01 linewidth 0.01
    replot
    reset
}
#   set terminal x11
