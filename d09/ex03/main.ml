let green = "\x1b[32m"
let reset = "\x1b[0m"

let add = ( + )
let div = ( / )
let eq = ( = )

let describe (print_fn, fn, fname) v =
  print_string "apply "; print_string fname; print_string " to "; print_fn v; print_char '\n';
  fn v

let recover err = Try.Try.return 150
let cmp = eq 150

let int_fns = [
  ( print_int, (fun e -> Try.Try.return (add 5 e)), "add 5" );
  ( print_int, (fun e -> Try.Try.return (add 10 e)), "add 10" );
  ( print_int, (fun e -> Try.Try.return (div 10 e)), "div 10" );
  ( print_int, (fun e -> Try.Try.return (div 0 e)), "div 0" );
  ( print_int, (fun e -> Try.Try.return (div 5 e)), "div 5" );
  ( print_int, (fun e -> Try.Try.return (add 100 e)), "add 100" )
]

let described_int_fns = List.map describe int_fns

let main () =
  print_endline "START = 10";
  let start = Try.Try.return 10 in
  let failed = List.fold_left (fun acc fn -> Try.Try.bind acc fn) start described_int_fns in
  print_endline "RECOVER WITH VALUE 150";
  let recovered = Try.Try.recover failed recover in
  let _fail_again = List.fold_left (fun acc fn -> Try.Try.bind acc fn) recovered described_int_fns in
  print_endline "FILTER FAIL (CMP START WITH 150)";
  let filter_fail = Try.Try.filter start cmp in
  let _ = List.fold_left (fun acc fn -> Try.Try.bind acc fn) filter_fail described_int_fns in
  print_endline "FILTER SUCCESS (CMP RECOVERED WITH 150)";
  let filter_success = Try.Try.filter recovered cmp in
  let _ = List.fold_left (fun acc fn -> Try.Try.bind acc fn) filter_success described_int_fns in
  print_endline "NEST FAILED";
  let nested_failure = Try.Try.return failed in
  let flatten_failure = Try.Try.flatten nested_failure in
  let _ = List.fold_left (fun acc fn -> Try.Try.bind acc fn) flatten_failure described_int_fns in
  print_endline "NEST START";
  let nested_start = Try.Try.return start in
  let flatten_start = Try.Try.flatten nested_start in
  let _ = List.fold_left (fun acc fn -> Try.Try.bind acc fn) flatten_start described_int_fns in
  print_endline "..."

let () = main ()