let () = Printexc.record_backtrace true

module SIA =
  struct
    include Array

    type t = int array
  end

module Safe = Test.Make(SIA)

let () =
  Alcotest.run "Safe Curve25519"
    [ "5 steps",  (Safe.tests 5);
      "10 steps", (Safe.tests 10);
      "20 steps", (Safe.tests 20);
      "40 steps", (Safe.tests 40);
      "80 steps", (Safe.tests 80); ]
