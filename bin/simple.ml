module A =
  struct
    include Array

    type t = int array

    let of_string str =
      let explode s =
        let rec exp i l =
          if i < 0 then l else exp (i - 2) ((String.sub s i 2) :: l) in
        exp (String.length s - 2) []
      in
      if String.length str = 32 * 2
      then explode str
           |> List.map (fun x -> "0x" ^ x)
           |> List.map int_of_string
           |> Array.of_list
      else raise (Invalid_argument "Curve25519.Array.of_string")

    let to_string arr =
      List.fold_left
        (fun acc x -> acc ^ Printf.sprintf "%02x" x)
        "" (Array.to_list arr)
  end

module Callipyge = Callipyge.Make(A)

let () =
  let r = Array.make 32 0 in
  if Array.length Sys.argv = 3
  then Callipyge.curve25519 r
    (A.of_string Sys.argv.(1))
    (A.of_string Sys.argv.(2)) |> fun _ -> Printf.printf "%s\n%!" (A.to_string r)
  else if Array.length Sys.argv = 2
  then Callipyge.curve25519_base r
    (A.of_string Sys.argv.(1)) |> fun _ -> Printf.printf "%s\n%!" (A.to_string r)
  else Printf.eprintf "%s [32-bytes] [32-bytes]?\n%!" Sys.argv.(0)
