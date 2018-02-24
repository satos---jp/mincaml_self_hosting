./main ../raytracer/min-rt.ml ../raytracer/globals.ml -o t.s -t -stack -asi
python tortesia2x86.py < t.s > out.s

