let main () =
  let crypted = Cipher.ft_crypt "hello" [Cipher.rot42; (Cipher.xor 18)] in
  print_endline crypted;
  print_endline (Uncipher.ft_uncrypt crypted [Uncipher.unrot42; (Uncipher.xor 18)])

let crypt_fn_from_str = function
 | "rot42" -> Cipher.rot42
 | "xor 1000" -> Cipher.xor 1000
 | "xor 5" -> Cipher.xor 1000
 | "xor 0" -> Cipher.xor 0
 | "ceasar 10" -> (fun s -> Cipher.ceasar s 10)
 | "ceasar 1000" -> (fun s -> Cipher.ceasar s 1000)
 | _ -> fun s -> s

let decrypt_fn_from_str = function
| "rot42" -> Uncipher.unrot42
| "xor 1000" -> Uncipher.xor 1000
| "xor 5" -> Uncipher.xor 1000
| "xor 0" -> Uncipher.xor 0
| "ceasar 10" -> (fun s -> Uncipher.unceasar s 10)
| "ceasar 1000" -> (fun s -> Uncipher.unceasar s 1000)
| _ -> fun s -> s

let rec crypt_fns_from_strs = List.map crypt_fn_from_str
let rec decrypt_fns_from_strs = List.map decrypt_fn_from_str

let rec print_list = function
 | h::[] -> print_string h
 | h::t -> print_string h; print_string ", "; print_list t
 | [] -> print_string "[]"

let test s fnames =
  Printf.printf "----ABOUT TO ENCRYPT [%s]----\n" s;
  Printf.printf "with the following functions: ";
  print_list fnames;
  print_char '\n';
  let crypt_fns = crypt_fns_from_strs fnames in
  let decrypt_fns = decrypt_fns_from_strs fnames in
  let crypted = Cipher.ft_crypt s crypt_fns in
  let decrypted = Uncipher.ft_uncrypt crypted decrypt_fns in
  Printf.printf "Encrypted: %s\n" crypted;
  Printf.printf "Decrypted: %s\n" decrypted

let () =
  test "hello" ["rot42"; "xor 1000"; "xor 5"];
  test "abcdefghijkl" ["ceasar 10"; "rot42"; "xor 0"; "xor 1000"];
  test "Lorem ipsum!@#$" ["ceasar 1000"; "xor 5"];
  test "                 " []