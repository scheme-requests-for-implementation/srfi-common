set output "/tmp/srfi.svg"
set grid xtics ytics
set terminal svg font "Arial,14" linewidth 3
set tics font "Arial,10"
set timefmt "%Y/%m/%d"
set title "Cumulative SRFIs"
set xdata time
set xtics "1998/1/1",31557600,"2017/12/31" nomirror rotate by -45
unset border
show grid
plot "/tmp/srfi-draft.dat" using 1:2 title "draft" with lines smooth cumulative, \
     "/tmp/srfi-final.dat" using 1:2 title "final" with lines smooth cumulative
