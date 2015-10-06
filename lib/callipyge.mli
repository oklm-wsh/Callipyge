val basev : int array
val minusp : int array
val add : int array -> int -> int array -> int -> int array -> int -> unit
val sub : int array -> int -> int array -> int -> int array -> int -> unit
val squeeze : int array -> int -> unit
val freeze : int array -> int -> unit
val mult : int array -> int -> int array -> int -> int array -> int -> unit
val mult21665 : int array -> int array -> unit
val square : int array -> int -> int array -> int -> unit
val select : int array -> int array -> int array -> int array -> int -> unit
val main_loop : int array -> bytes -> unit
val recip : int array -> int -> int array -> int -> unit
val crypto_scalar_mult : Bytes.t -> Bytes.t -> int array -> int
val crypto_scalar_mult' : Bytes.t -> Bytes.t -> Bytes.t -> int
val crypto_scalar_mult_base : Bytes.t -> Bytes.t -> int
