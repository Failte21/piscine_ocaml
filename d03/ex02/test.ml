let main () =
  print_endline "----ABOUT TO ENCRYPT [hello]----";
  let crypted = Cipher.ft_crypt "hello" [Cipher.rot42; (Cipher.xor 18)] in
  print_endline crypted;
  print_endline (Uncipher.ft_uncrypt crypted [Uncipher.unrot42; (Uncipher.xor 18)])

let () = main ()