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

print_int (fneg 1.0);
print_int (fneg 0.0);
print_int (fneg (-1.0));

(*
print_int (fless 1.0 1.5);
print_int (fless 1.5 1.0);
print_int (fless 1.0 1.0);
*)
print_char 10
