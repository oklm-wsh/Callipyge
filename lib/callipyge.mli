type _ key = private int array
(** Type of keys. *)

and public
and secret
and shared

val base: public key

val secret_key_of_string: string -> secret key
val secret_key_of_int_array: int array -> secret key
val public_key_of_string: string -> public key
val public_key_of_int_array: int array -> public key
val string_of_key: _ key -> string

val ecdh: out:int array -> secret:secret key -> public:public key -> unit
val ecdh_base: out:int array -> secret:secret key -> unit
val public_of_secret: secret key -> public key
val shared: secret:secret key -> public:public key -> shared key

val public_key_of_shared: shared key -> public key
val secret_key_of_shared: shared key -> secret key

val pp_public_key: public key Fmt.t
val pp_shared_key: shared key Fmt.t

val equal_key: 'a key -> 'a key -> bool
