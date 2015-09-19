
# call me: gnuplot -e "filename='data/testing/errors.csv'" util/plot_errors_with_bars.plt

set terminal qt persist

plot filename using 1:2 with lines title 'mean error before iteration',\
     filename using 1:5 with lines title 'mean error after fitting as',\
     filename using 1:8 with lines title 'mean error after updating phis',\
     filename using 1:2:3:4  with errorbars notitle,\
     filename using 1:5:6:7  with errorbars notitle,\
     filename using 1:8:9:10 with errorbars notitle


pause -1
