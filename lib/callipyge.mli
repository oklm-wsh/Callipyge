(** Callipyge, Curve25519 implementation in OCaml. *)

type _ key = private int array
(** Type of keys. *)

and public
and secret

val base: public key

type ctx

val make_ctx: unit -> ctx
val reset_ctx: ctx -> unit

val secret_key_of_string: string -> secret key
val public_key_of_string: string -> public key
val string_of_key: _ key -> string

val ecdh: ctx:ctx -> out:int array -> secret:secret key -> public:public key -> unit
val ecdh_base: ctx:ctx -> out:int array -> secret:secret key -> unit
val public_of_secret: ctx:ctx -> secret key -> public key
val shared: ctx:ctx -> secret:secret key -> public:public key -> public key

val pp_key: public key Fmt.t

val equal_key: 'a key -> 'a key -> bool
