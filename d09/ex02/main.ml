let green = "\x1b[32m"
let reset = "\x1b[0m"

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []

let ints_s = [1; 10; -5; -10; 100; 0]
let floats_s = [1.0; 0.2; -5.5; -1.111; 0.0000001; 0.0]

let floats_pos = List.filter (fun e -> e >= 0.0) floats_s
let ints_pos = List.filter (fun e -> e >= 0) ints_s

let powers = (-3)--(3)
let pos_powers = 0--5

let ints = [
  (1, -5);
  (10, 2);
  (-11, -150);
  (-666, 666);
  (1000000, 55);
]

let floats = [
  (1.125, -5.3);
  (10.2, 0.02);
  (-11.0147, -0.00150);
  (-666.666, 666.666);
  (0.0000001, 55.0);
]

let float_pow_fn = ("FLOAT POWER", Calc.CalcFloat.power, " ^ ")
let int_pow_fn = ("INT POWER", Calc.CalcInt.power, " ^ ")
let float_fac_fn = ("FLOAT Factorial", Calc.CalcFloat.fact)
let int_fac_fn = ("INT Factorial", Calc.CalcInt.fact)

let float_fns = [
  ("FLOAT ADD", Calc.CalcFloat.add, " + ");
  ("FLOAT SUB", Calc.CalcFloat.sub, " - ");
  ("FLOAT DIV", Calc.CalcFloat.div, " / ");
  ("FLOAT MUL", Calc.CalcFloat.mul, " * ")
]

let int_fns = [
  ("INT ADD", Calc.CalcInt.add, " + ");
  ("INT SUB", Calc.CalcInt.sub, " - ");
  ("INT DIV", Calc.CalcInt.div, " / ");
  ("INT MUL", Calc.CalcInt.mul, " * ")
]

let __test values print_fn (title, fn, symbol) =
  Printf.printf "%s-----%s-----%s\n" green title reset;
  List.iter (fun (a, b) -> (
    print_fn a; print_string symbol; print_fn b; print_string " = ";
    print_fn (fn a b);
    print_char '\n'
  )) values

let _test (values, print_fn, fns_with_titles) =
  List.iter (__test values print_fn) fns_with_titles

let test_pow powers values print_fn (title, fn, symbol) =
  Printf.printf "%s-----%s-----%s\n" green title reset;
  List.iter (fun a -> (
    List.iter(fun b -> (
      print_fn a; print_string symbol; print_int b; print_string " = ";
      print_fn (fn a b);
      print_char '\n'
    )) powers
  )) values

let test_fac values print_fn (title, fn) =
  Printf.printf "%s-----%s-----%s\n" green title reset;
  List.iter(fun a -> (
    print_string "Factorial of "; print_fn a; print_string ": ";
    print_fn (fn a);
    print_char '\n'
  )) values

let test =
  _test (ints, print_int, int_fns);
  _test (floats, print_float, float_fns);
  test_pow powers floats_s print_float float_pow_fn;
  test_pow pos_powers ints_s print_int int_pow_fn;
  test_fac floats_pos print_float float_fac_fn;
  test_fac (0--15) print_int int_fac_fn

let main () = test

let () = main ()