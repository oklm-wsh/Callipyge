let to_string bytes =
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []
  in
  List.fold_left
    (fun acc x -> acc ^ Printf.sprintf "%02x" (Char.code x))
    "" (explode bytes)

let set_all f s =
  Bytes.iteri (fun idx chr -> Bytes.set s idx (f idx chr)) s

let () = Printexc.record_backtrace true

let setter e1 e2 k e1k e2k =
  let module M =
    struct
      type t = Bytes.t

      let pp fmt x = Format.pp_print_string fmt (Bytes.to_string x)

      let equal e1e2k e2e1k =
        let result = Bytes.compare e1e2k e2e1k = 0 in
        if result
        then begin
          set_all (fun idx chr ->
            (Char.code chr)
            lxor (Bytes.get e2k idx |> Char.code) |> Char.chr) e1;
          set_all (fun idx chr ->
            (Char.code chr)
            lxor (Bytes.get e1k idx |> Char.code) |> Char.chr) e2;
          set_all (fun idx chr ->
            (Char.code chr)
            lxor (Bytes.get e1e2k idx |> Char.code) |> Char.chr) k;
        end; result
    end
  in (module M : Alcotest.TESTABLE with type t = M.t)

let make_step e1 e2 k =
  Printf.sprintf "curve(e2, curve25519(e1, k)) = \
                  curve25519(e1, curve25519(e2, k))",
  `Quick,
  (fun () ->
    Printf.fprintf stderr "k:  %s\n%!" (to_string k);
    Printf.fprintf stderr "e1: %s\n%!" (to_string e1);
    Printf.fprintf stderr "e2: %s\n%!" (to_string e2);
    let e1k = Bytes.make 32 (Char.chr 0) in
    let e2k = Bytes.make 32 (Char.chr 0) in
    let e1e2k = Bytes.make 32 (Char.chr 0) in
    let e2e1k = Bytes.make 32 (Char.chr 0) in

    let _ = Callipyge.crypto_scalar_mult' e1k e1 k in
    let _ = Callipyge.crypto_scalar_mult' e2k e2 k in
    let _ = Callipyge.crypto_scalar_mult' e1e2k e2 e1k in
    let _ = Callipyge.crypto_scalar_mult' e2e1k e1 e2k in

    Alcotest.(check (setter e1 e2 k e1k e2k)) "equality"
      e1e2k e2e1k)

let tests n =
  let e1 = Bytes.init 32 (function 0 -> Char.chr 3 | _ -> Char.chr 0) in
  let e2 = Bytes.init 32 (function 0 -> Char.chr 5 | _ -> Char.chr 0) in
  let k =  Bytes.init 32 (function 0 -> Char.chr 9 | _ -> Char.chr 0) in

  let rec aux acc = function
    | 0 -> acc
    | n -> aux ((make_step e1 e2 k) :: acc) (n - 1)
  in aux [] n

let () =
  Alcotest.run "Curve25519"
    [ "5 steps",  (tests 5);
      "10 steps", (tests 10);
      "20 steps", (tests 20);
      "40 steps", (tests 40);
      "80 steps", (tests 80); ]
