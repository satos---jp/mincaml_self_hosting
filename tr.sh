./main ../raytracer/min-rt.ml ../raytracer/globals.ml -o t.s -t
python tortesia2x86.py $1 < t.s > out.s

