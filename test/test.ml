let pp_base ppf arr =
  for i = 0 to Array.length arr - 1
  do Fmt.pf ppf "%02X" (Array.get arr i) done

type public_key = Callipyge.public Callipyge.key
type secret_key = Callipyge.secret Callipyge.key

let pp_secret ppf (arr:secret_key) = pp_base ppf (arr :> int array)

let ctx = Callipyge.make_ctx ()

let doit out (secret: secret_key) (public: public_key) =
    Fmt.pr "%a %a " pp_secret secret Callipyge.pp_key public
  ; Callipyge.ecdh ~ctx ~out ~secret ~public
  ; Fmt.pr "%a\n%!" pp_base out

let ecdh (e1: secret_key) (e2: secret_key) (k: public_key) e1k e2k =
  let equal e1e2k e2e1k =
    let result =
      assert (Array.length e1e2k = 32 && Array.length e2e1k = 32);
      let rt = ref 0 in
      for i = 0 to 31 do rt := !rt lor (Array.get e1e2k i lxor Array.get e2e1k i) done;
      !rt = 0 in

    Fmt.epr "%a equal %a: %b.\n%!"
      pp_base e1e2k
      pp_base e2e1k result;

    if result
    then begin
        Array.iteri (fun i x -> Array.set (e1 :> int array) i (x lxor (Array.get (e2k   :> int array) i))) (e1 :> int array)
      ; Array.iteri (fun i x -> Array.set (e2 :> int array) i (x lxor (Array.get (e1k   :> int array) i))) (e2 :> int array)
      ; Array.iteri (fun i x -> Array.set (k  :> int array) i (x lxor (Array.get e1e2k                i))) (k  :> int array)
      ; result
    end else result in
  let pp = pp_base in
  Alcotest.testable pp equal

let step (e1: secret_key) (e2: secret_key) (k: public_key) =
  let ecdh = ecdh e1 e2 k in

  "ecdh(e2, ecdh(e1, k)) = ecdh(e1, ecdh(e2, k))", `Quick,
  fun () ->
    let e1k   = Array.make 32 0 in
    let e2k   = Array.make 32 0 in
    let e1e2k = Array.make 32 0 in
    let e2e1k = Array.make 32 0 in

    let () = doit e1k e1 k in
    let () = doit e2e1k e2 (Obj.magic e1k) in
    let () = doit e2k e2 k in
    let () = doit e1e2k e1 (Obj.magic e2k) in

    Alcotest.(check (ecdh e1k e2k)) "equal" e1e2k e2e1k

let e1 = "\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
let e2 = "\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
let k  = "\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let (<.>) f g = fun x -> f (g x)

let tests n =
  let e1 = Callipyge.secret_key_of_string e1 in
  let e2 = Callipyge.secret_key_of_string e2 in
  let k  = Callipyge.public_key_of_string k  in
  let step = step e1 e2 k in

  let rec go acc = function
    | 0 -> acc | n -> go (step :: acc) (pred n) in
  go [] n

let () =
  Alcotest.run "ECDH"
    [ "5 steps", tests 5
    ; "10 steps", tests 10
    ; "20 steps", tests 20
    ; "40 steps", tests 40
    ; "80 steps", tests 80
    ; "160 steps", tests 160 ]
