type _ key = private int array
type public
type secret

val secret_key_of_string: string -> secret key
val public_key_of_string: string -> public key
val string_of_key: _ key -> string

val ecdh: int array -> secret key -> public key -> unit
val ecdh_base: int array -> secret key -> unit
val public_of_secret: secret key -> public key
val shared: secret key -> public key -> public key

val pp_key: public key Fmt.t

val equal_key: 'a key -> 'a key -> bool
