set terminal png
set output 'image.png'
set title "This is my title"
set xlabel "x"
set ylabel "y"
plot "sinus.dat" using 1:2 with lines
q
