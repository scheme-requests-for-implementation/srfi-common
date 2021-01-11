set format x "%Y-%m-%d"
set output "/tmp/srfi.svg"
set grid xtics ytics
set style line 1 lt rgb "black" lw 0.5
set style line 2 lt rgb "#800080" lw 0.5
set style line 3 lt rgb "gray" lw 0.5
set terminal svg font "Arial,14" linewidth 3
set tics font "Arial,10"
set timefmt "%Y-%m-%d"
set xdata time
set xtics "1998-01-01",31557600,"2021-12-31" nomirror rotate by -45
unset border
show grid
plot "/tmp/srfi-all.dat" using 1:2 title "all" with lines ls 1, \
     "/tmp/srfi-final.dat" using 1:2 title "final" with lines ls 2, \
     "/tmp/srfi-withdrawn.dat" using 1:2 title "withdrawn" with lines ls 3