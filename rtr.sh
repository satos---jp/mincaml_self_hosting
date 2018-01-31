./main ../raytracer/min-rt.ml ../raytracer/globals.ml -o t.s -t -asi -v > o.txt
python tortesia2x86.py -r < t.s > out.s
echo "compiled" >&2

