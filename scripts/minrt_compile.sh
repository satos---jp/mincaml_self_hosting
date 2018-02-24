./main ../raytracer/min-rt.ml ../raytracer/globals.ml -o o_tortesia.s -t -v > o.txt
python ./scripts/tortesia2x86.py -r < o_tortesia.s > out.s
echo "compiled mincaml to asm" >&2

