#FFT de la fente
 i=2
#  set terminal png
#  set output 'fente_'.i.'_FFT.png'

set pm3d map
splot 'file'.i.'.data' u 1:2:4 with points pointtype 5 pointsize 1 palette linewidth 30
set xlabel ' x( en USI)'
set ylabel ' y( en USI)'
set title 'FFT de la fente'.i
replot

#set terminal x11