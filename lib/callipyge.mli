(** {1 Curve25519}

   Each Curve25519 user has a 32-byte secret key and a 32-byte public key. Each
   set of two Curve25519 users has a 32-byte shared secret used to authenticate
   and encrypt messages between the two users.

   Alice has a secret-key [a].
   Bob has a secret-key [b].
   [9] is base key - in this implementation
     curve25519(a, b, [9]) = curve25519_base(a, b)

   \{ Alice, Bob \}'s shared secret: curve25519(a, curve25519(b, 9))
   \{ Bob, Alice \}'s shared secret: curve25519(b, curve25519(a, 9))

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
    (**  [t] is an abstract array type.
         The implementation must be able to support a [length] of 96,
         and values in the range (0 <= value <= 255).
         [t] is assumed to be in little-endian encoding (LSB first).*)

    val get : t -> int -> int
    (** [get t offset] is the value contained at [offset].*)
    val set : t -> int -> int -> unit
    (** [set t offset value] sets the cell at [offset] (0-indexed) to [value].*)
    val sub : t -> int -> int -> t
    (** [sub t offset length] must return a new t of [length] elements,
        containing the slice of [t] starting at [offset].
        NOTE that this new value is mutated, so the returned [t] MUST NOT share
             the underlying storage with the original [t].*)
    val init : int -> (int -> int) -> t
    (** [init length ([offset] -> [value])] initializes a new array.*)
    val make : int -> int -> t
    (** [make length value] initializes a new array consisting of [length] cells
        containing [value].
        This function is equivalent to [Array.(init length (fun _ -> value))].*)
  end

module Make (X : Array) :
  sig
    val curve25519 : X.t -> X.t -> X.t -> int
    (** [curve25519 q n p] computes the shared secret between secret key [n]
       and public key [p]. The result is stored in [q].
       [curve25519] always returns 0.*)

    val curve25519_base : X.t -> X.t -> int
    (** [curve25519_base q n] is equivalent to [curve25519] with secret key [n]
        and the base point (9), with the resulting public key stored in [q].*)
  end
