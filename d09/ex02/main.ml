let green = "\x1b[32m"
let reset = "\x1b[0m"

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

let test =
  _test (ints, print_int, int_fns);
  _test (floats, print_float, float_fns)

let main () = test

let () = main ()