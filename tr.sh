./main min-rt.ml ../raytracer/globals.ml -o t.s -t -noinline
python tortesia2x86.py < t.s > out.s

