print_int (int_of_float 3.14);
print_int (int_of_float 3.92);
print_int (int_of_float 3.0);
print_int (int_of_float 0.0);
print_int (int_of_float (-3.14));
print_int (int_of_float (-3.92));
print_int (int_of_float (-3.0));
print_char 10;

print_int (fiszero 0.5);
print_int (fiszero 0.0);
print_int (fiszero (-0.5));
print_char 10;

print_int (fisneg 1.0);
print_int (fisneg 0.0);
print_int (fisneg (-1.0));
print_char 10;

print_int (fispos 1.0);
print_int (fispos 0.0);
print_int (fispos (-1.0));
print_char 10;

print_int (int_of_float (0.0));
print_int (int_of_float (3.5));
print_int (int_of_float (3.49));
print_int (int_of_float (-3.55));
print_int (int_of_float (-3.5));
print_int (int_of_float (-3.49));
print_char 10;

print_int (int_of_float (fhalf 3.14));
print_int (int_of_float (fhalf 13.0));
print_int (int_of_float (fhalf 5.21));
print_char 10;

print_int (int_of_float ((floor 3.14) *. 10.0));
print_int (int_of_float ((floor 1.59) *. 10.0));
print_int (int_of_float ((floor 5.12) *. 10.0));
print_char 10;

print_int (int_of_float (fabs 3.14));
print_int (int_of_float (fabs 0.0));
print_int (int_of_float (fabs (-5.21)));
print_char 10;


print_int (int_of_float (fneg 1.1));
print_int (int_of_float (fneg 0.0));
print_int (int_of_float (fneg (-1.1)));
print_char 10;

print_int (int_of_float ((sin 2.34) *. 1000.0));
print_int (int_of_float ((sin (-3.51)) *. 1000.0));
print_int (int_of_float ((sin 100.0) *. 1000.0));
print_char 10;

print_int (int_of_float ((cos 2.34) *. 1000.0));
print_int (int_of_float ((cos (-3.51)) *. 1000.0));
print_int (int_of_float ((cos 100.0) *. 1000.0));
print_char 10;

print_int (int_of_float ((atan 2.34) *. 1000.0));
print_int (int_of_float ((atan (-3.51)) *. 1000.0));
print_int (int_of_float ((atan 100.0) *. 1000.0));
print_char 10;


print_int (int_of_float ((fsqr 256.0) *. 1000.0));
print_int (int_of_float ((fsqr 314.0) *. 1000.0));
print_int (int_of_float ((fsqr 278.0) *. 1000.0));
print_char 10;

print_int (int_of_float ((sqrt 256.34) *. 1000.0));
print_int (int_of_float ((sqrt 314.51) *. 1000.0));
print_int (int_of_float ((sqrt 278.239) *. 1000.0));
print_char 10;


print_int (if (fless 1.0 1.5) then 1 else 0);
print_int (if (fless 1.5 1.5) then 1 else 0);
print_int (if (fless 1.5 1.0) then 1 else 0);
print_int (if (fless 1.0 (-1.5)) then 1 else 0);
print_int (if (fless (-1.5) (-1.5)) then 1 else 0);
print_int (if (fless (-1.5) 1.0) then 1 else 0);
print_char 10;

print_int (int_of_float ((float_of_int 5) *. (float_of_int 5)));
print_int (int_of_float ((float_of_int 5) *. (float_of_int 5)));
print_int (int_of_float ((float_of_int 5) *. (float_of_int 5)));
print_char 10;

print_char 10


