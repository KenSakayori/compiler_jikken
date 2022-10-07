let sum =
"let rec sum x =
  if x <= 0 then 0 else
  sum (x - 1) + x in
print_int (sum 10000)
"

let sum_e =
"let rec sum x =
  if x <= 0 then 0 else
  sum (x -) + x in
print_int (sum 10000)
"

let fib =
"let rec fib n =
  if n <= 1 then n else
  fib (n - 1) + fib (n - 2) in
print_int (fib 30)
"

let fib_e =
"let rec fib n =
  if 1 then n else
  fib (n - 1) + fib (n - 2) in
print_int (fib 30)
"
