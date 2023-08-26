set output "/tmp/durations.svg"
set style fill solid
set terminal svg font "Arial,14"
set tics font "Arial,10"
set xtics rotate by 45 offset 0,-1 nomirror
set xlabel "SRFI number"
set ylabel "days between first draft and finalization"
set yrange [0:700]
unset border
unset key
plot "/tmp/srfi-durations.txt" using 2:xtic(int($1)%10==0 ? strcol(1):"") with boxes linecolor rgb "black"