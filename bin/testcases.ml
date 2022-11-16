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

let tuple =
"let a = Array.make 1 (1, (2, 3)) in
let i = 0 in
let b = a.(i) in
let (x, c) = b in
let (y, z) = c in
print_int x; print_int y; print_int z; print_newline ()
"

let tuple2 =
"let a = (1, (2, 3)) in
let (x, b) = a in
let (y, z) = b in
print_int x; print_int y; print_int z; print_newline ()
"

let fun_ =
"let a = Array.make 2 1 in
a.(1) <- 3;
let x = a.(0) in
let y = a.(1) in
let z = (fun s t -> s + t) x y in
print_int z;
print_newline ()
"

let partial =
"let a = Array.make 2 1 in
a.(1) <- 3;
let x = a.(0) in
let y = a.(1) in
let rec f x y = x + y in
let g = f x in
print_int (g y);
print_newline ()
"
