(** {1 Introduction}

   Each Curve25519 user has a 32-byte secret key and a 32-byte public key. Each
   set of two Curve25519 users has a 32-byte shared secret used to authenticate
   and encrypt messages between the two users.

   Alice has a secret-key [a].
   Bob has a secret-key [b].
   [9] is base key - in this implementation
     curve25519(a, b, [9]) = curve25519_base(a, b)

   { Alice, Bob }'s shared secret: curve25519(a, curve25519(b, 9))
   { Bob, Alice }'s shared secret: curve25519(b, curve25519(a, 9))

   and curve25519(a, curve25519(b, 9)) = curve25519(b, curve25519(a, 9))

   A hash of the shared secret curve25519(a, curve25519(b, 9)) is used as the
   key for a secret-key authentification system (to authenticate messages), or
   as the key for a secret-key authenticated-encryption system (to
   simultaneously encrypt and authenticate messages).

   The curve25519 function is Fp-restricted x-coordinate scalar multiplication
   on E(Fp^2), where p is the prime number 2^255 - 19 and E is the elliptic
   curve y^2 = x^3 + 486662 * x^2 + x^2.
*)

module type Array =
  sig
    type t

    val get : t -> int -> int
    val set : t -> int -> int -> unit
    val sub : t -> int -> int -> t
    val init : int -> (int -> int) -> t
    val make : int -> int -> t
  end

module Make (X : Array) :
  sig
    val curve25519 : X.t -> X.t -> X.t -> int
    val curve25519_base : X.t -> X.t -> int
  end
