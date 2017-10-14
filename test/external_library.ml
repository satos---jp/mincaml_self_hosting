print_int (int_of_float 3.14);
print_int (int_of_float 3.92);
print_int (int_of_float 3.0);
print_int (int_of_float 0.0);
print_int (int_of_float (-3.14));
print_int (int_of_float (-3.92));
print_int (int_of_float (-3.0));

print_int (fiszero 1.0);
print_int (fiszero 0.0);
print_int (fiszero (-1.0));

print_int (fisneg 1.0);
print_int (fisneg 0.0);
print_int (fisneg (-1.0));

print_int (fispos 1.0);
print_int (fispos 0.0);
print_int (fispos (-1.0));

print_int (int_of_float (fhalf 3.14));
print_int (int_of_float (fhalf 13.0));
print_int (int_of_float (fhalf 5.21));

print_int (int_of_float (0.0));
print_int (int_of_float (3.5));
print_int (int_of_float (3.49));
print_int (int_of_float (-3.55));
print_int (int_of_float (-3.5));
print_int (int_of_float (-3.49));

print_int (int_of_float (fneg 1.1));
print_int (int_of_float (fneg 0.0));
print_int (int_of_float (fneg (-1.1)));

print_int (int_of_float ((sin 2.34) *. 1000.0));
print_int (int_of_float ((sin (-3.51)) *. 1000.0));
print_int (int_of_float ((sin 0.239) *. 1000.0));

print_int (int_of_float ((cos 2.34) *. 1000.0));
print_int (int_of_float ((cos (-3.51)) *. 1000.0));
print_int (int_of_float ((cos 0.239) *. 1000.0));

print_int (int_of_float ((atan 2.34) *. 1000.0));
print_int (int_of_float ((atan (-3.51)) *. 1000.0));
print_int (int_of_float ((atan 0.239) *. 1000.0));


print_int (int_of_float ((fsqr 2.34) *. 1000.0));
print_int (int_of_float ((fsqr 3.51) *. 1000.0));
print_int (int_of_float ((fsqr 0.239) *. 1000.0));

print_int (int_of_float ((sqrt 2.34) *. 1000.0));
print_int (int_of_float ((sqrt 3.51) *. 1000.0));
print_int (int_of_float ((sqrt 0.239) *. 1000.0));

(*
print_int (fless 1.0 1.5);
print_int (fless 1.5 1.0);
print_int (fless 1.0 1.0);
*)
print_char 10
