let random_key len =
  let ic = open_in_bin "/dev/urandom" in
  let rs = really_input_string ic len in
  close_in ic; rs

let bench () =
  let secret = Callipyge.secret_key_of_string (random_key 32) in
  let public = Callipyge.public_key_of_string (random_key 32) in

  let f () = Callipyge.shared ~secret ~public in

  Benchmark.throughputN 1 [ "callipyge", f, () ]

let () =
  let open Benchmark.Tree in

  register @@ "callipyge" @>>> [ "ecdh" @> lazy (bench ()) ]

let () = Benchmark.Tree.run_global ()
