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
set xtics "1998-01-01",31557600,"2100-12-31" nomirror rotate by -45
unset border
show grid
plot "/tmp/srfi-all-counts.txt" using 1:2 smooth cumulative title "all" with lines ls 1, \
     "/tmp/srfi-final-counts.txt" using 1:2 smooth cumulative title "final" with lines ls 2, \
     "/tmp/srfi-withdrawn-counts.txt" using 1:2 smooth cumulative title "withdrawn" with lines ls 3