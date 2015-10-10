let () = Printexc.record_backtrace true

module UIA =
  struct
    include Array

    type t = int array

    let get = unsafe_get
    let set = unsafe_set
  end

module Unsafe = Test.Make(UIA)

let () =
  Alcotest.run "Unsafe Curve25519"
    [ "5 steps",  (Unsafe.tests 5);
      "10 steps", (Unsafe.tests 10);
      "20 steps", (Unsafe.tests 20);
      "40 steps", (Unsafe.tests 40);
      "80 steps", (Unsafe.tests 80); ]
