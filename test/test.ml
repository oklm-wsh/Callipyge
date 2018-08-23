let pp_base ppf arr =
  for i = 0 to Array.length arr - 1
  do Fmt.pf ppf "%02X" (Array.get arr i) done

let doit ek e k =
    Fmt.pr "%a %a " Callipyge.pp_key (Callipyge.public_of_secret e) pp_base k
  ; Callipyge.ecdh ek e k
  ; Fmt.pr "%a\n%!" Callipyge.pp_key ek

type public_key = Callipyge.public Callipyge.key
type secret_key = Callipyge.secret Callipyge.key

let ecdh (e1: secret_key) (e2: secret_key) k e1k e2k =
  let equal (e1e2k: public_key) (e2e1k: public_key) =
    let result = Callipyge.equal_key e1e2k e2e1k in

    Fmt.epr "%a equal %a: %b.\n%!" Callipyge.pp_key e1e2k Callipyge.pp_key e2e1k (Callipyge.equal_key e1e2k e2e1k);

    if result
    then begin
        Array.iteri (fun i x -> Array.set (e1 :> int array) i (x lxor (Array.get (e2k :> int array) i))) (e1 :> int array)
      ; Array.iteri (fun i x -> Array.set (e2 :> int array) i (x lxor (Array.get (e1k :> int array) i))) (e2 :> int array)
      ; Array.iteri (fun i x -> Array.set k i (x lxor (Array.get (e1e2k :> int array) i))) k
      ; result
    end else result in
  let pp = Callipyge.pp_key in
  Alcotest.testable pp equal

let step e1 e2 k =
  let ecdh = ecdh e1 e2 k in

  "ecdh(e2, ecdh(e1, k)) = ecdh(e1, ecdh(e2, k))", `Quick,
  fun () ->
    let e1k   = Callipyge.public_key_of_string (String.make 32 '\000') in
    let e2k   = Callipyge.public_key_of_string (String.make 32 '\000') in
    let e1e2k = Callipyge.public_key_of_string (String.make 32 '\000') in
    let e2e1k = Callipyge.public_key_of_string (String.make 32 '\000') in

    let () = doit e1k e1 k in
    let () = doit e2e1k e2 (e1k :> int array) in
    let () = doit e2k e2 k in
    let () = doit e1e2k e2 (e2k :> int array) in

    Alcotest.(check (ecdh (e1k :> int array) (e2k :> int array))) "equal" e1e2k e2e1k

let e1 = "\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
let e2 = "\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
let k  = "\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let (<.>) f g = fun x -> f (g x)

let tests n =
  let e1 = Callipyge.secret_key_of_string e1 in
  let e2 = Callipyge.secret_key_of_string e2 in
  let k = Array.init 32 (Char.code <.> String.get k) in
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
