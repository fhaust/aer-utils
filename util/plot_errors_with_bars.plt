
# call me: gnuplot -e "filename='data/testing/errors.csv'" util/plot_errors_with_bars.plt

set terminal qt persist
#set terminal wxt enhanced font 'Verdana,10' persist

plot filename using 1:2:3:4 with errorbars ls 1 notitle,\
     filename using 1:2 with lines ls 1 title 'mean error before iteration',\
     filename using 1:5:6:7 with errorbars ls 2 notitle,\
     filename using 1:5 with lines ls 2 title 'mean error after fitting as',\
     filename using 1:8:9:10 with errorbars ls 3 notitle,\
     filename using 1:8 with lines ls 3 title 'mean error after updating phis'

set title title
#set xrange [0:100]

replot

pause -1
#reread
