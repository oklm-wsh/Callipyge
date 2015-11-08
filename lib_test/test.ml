module type Array =
  sig
    include Callipyge.Array

    val length : t -> int
    val iteri : (int -> int -> unit) -> t -> unit
  end

module Make (X : Array) =
  struct
    let to_string bytes =
      let explode s =
        let rec exp i l =
          if i < 0 then l else exp (i - 1) ((X.get s i) :: l) in
        exp (X.length s - 1) []
      in
      List.fold_left
        (fun acc x -> acc ^ Printf.sprintf "%02x" x)
        "" (explode bytes)

    let set_all f s =
      X.iteri (fun idx code -> X.set s idx (f idx code)) s

    module Callipyge = Callipyge.Make(X)

    let setter e1 e2 k e1k e2k =
      let module M =
        struct
          type t = X.t

          let pp fmt x = Format.pp_print_string fmt (to_string x)

          let equal e1e2k e2e1k =
            let result = e1e2k = e2e1k in
            if result
            then begin
              set_all (fun idx code ->
                code
                lxor (X.get e2k idx)) e1;
              set_all (fun idx code ->
                code
                lxor (X.get e1k idx)) e2;
              set_all (fun idx code ->
                code
                lxor (X.get e1e2k idx)) k;
            end; result
        end
      in (module M : Alcotest.TESTABLE with type t = M.t)


    let make_step e1 e2 k =
      Printf.sprintf "curve(e2, curve(e1, k)) = \
                      curve(e1, curve(e2, k))",
      `Quick,
      (fun () ->
        Printf.fprintf stderr "k : %s\n%!" (to_string k );
        Printf.fprintf stderr "e1: %s\n%!" (to_string e1);
        Printf.fprintf stderr "e2: %s\n%!" (to_string e2);
        let e1k = X.make 32 0 in
        let e2k = X.make 32 0 in
        let e1e2k = X.make 32 0 in
        let e2e1k = X.make 32 0 in

        let _ = Callipyge.curve25519 e1k e1 k in
        let _ = Callipyge.curve25519 e2k e2 k in
        let _ = Callipyge.curve25519 e1e2k e2 e1k in
        let _ = Callipyge.curve25519 e2e1k e1 e2k in

        Alcotest.(check (setter e1 e2 k e1k e2k)) "equality"
          e1e2k e2e1k)

    let tests n =
      let e1 = X.init 32 (function 0 -> 3 | _ -> 0) in
      let e2 = X.init 32 (function 0 -> 5 | _ -> 0) in
      let k =  X.init 32 (function 0 -> 9 | _ -> 0) in

      let rec aux acc = function
        | 0 -> acc
        | n -> aux ((make_step e1 e2 k) :: acc) (n - 1)
      in aux [] n
  end
