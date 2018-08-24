let basev =
  Array.init 32 (function 0 -> 9 | _ -> 0)

let minusp =
  Array.init 32 (function 0 -> 19 | 31 -> 128 | _ -> 0)

type ctx =
  { xzm1     : int array
  ; xzm      : int array
  ; xzmb     : int array
  ; xzm1b    : int array
  ; xznb     : int array
  ; xzn1b    : int array
  ; a0       : int array
  ; a1       : int array
  ; b0       : int array
  ; b1       : int array
  ; c1       : int array
  ; r        : int array
  ; s        : int array
  ; t        : int array
  ; u        : int array

  ; z2       : int array
  ; z9       : int array
  ; z11      : int array
  ; z2_5_0   : int array
  ; z2_10_0  : int array
  ; z2_20_0  : int array
  ; z2_50_0  : int array
  ; z2_100_0 : int array
  ; t0       : int array
  ; t1       : int array }

let make_ctx () =
  { xzm1     = Array.make 64 0
  ; xzm      = Array.make 64 0
  ; xzmb     = Array.make 64 0
  ; xzm1b    = Array.make 64 0
  ; xznb     = Array.make 64 0
  ; xzn1b    = Array.make 64 0
  ; a0       = Array.make 64 0
  ; a1       = Array.make 64 0
  ; b0       = Array.make 64 0
  ; b1       = Array.make 64 0
  ; c1       = Array.make 64 0
  ; r        = Array.make 32 0
  ; s        = Array.make 32 0
  ; t        = Array.make 32 0
  ; u        = Array.make 32 0

  ; z2       = Array.make 32 0
  ; z9       = Array.make 32 0
  ; z11      = Array.make 32 0
  ; z2_5_0   = Array.make 32 0
  ; z2_10_0  = Array.make 32 0
  ; z2_20_0  = Array.make 32 0
  ; z2_50_0  = Array.make 32 0
  ; z2_100_0 = Array.make 32 0
  ; t0       = Array.make 32 0
  ; t1       = Array.make 32 0 }

let reset_ctx_main_loop, reset_ctx_recip =
  let reset a = Array.fill a 0 (Array.length a) 0 in
  (fun ctx ->
    reset ctx.xzm1
  ; reset ctx.xzm1
  ; reset ctx.xzm
  ; reset ctx.xzmb
  ; reset ctx.xzm1b
  ; reset ctx.xznb
  ; reset ctx.xzn1b
  ; reset ctx.a0
  ; reset ctx.a1
  ; reset ctx.b0
  ; reset ctx.b1
  ; reset ctx.c1
  ; reset ctx.r
  ; reset ctx.s
  ; reset ctx.t
  ; reset ctx.u),

  (fun ctx ->
    reset ctx.z2
  ; reset ctx.z9
  ; reset ctx.z11
  ; reset ctx.z2_5_0
  ; reset ctx.z2_10_0
  ; reset ctx.z2_20_0
  ; reset ctx.z2_50_0
  ; reset ctx.z2_100_0
  ; reset ctx.t0
  ; reset ctx.t1)

let reset_ctx ctx =
    reset_ctx_main_loop ctx
  ; reset_ctx_recip ctx

module A : sig
  type 'rw t constraint 'rw = [< `Rd | `Wr ]
  type ro = [ `Rd ]
  type wo = [ `Wr ]
  type rw = [ ro | wo ]

  val rw: int array -> rw t
  val ro: int array -> ro t
  val wo: int array -> wo t

  val protect_ro: [> ro ] t -> ro t
  val protect_wo: [> wo ] t -> wo t

  val get: [> ro ] t -> int -> int
  val set: [> wo ] t -> int -> int -> unit
  val length: [> ro ] t -> int
end = struct
  type 'rw t = int array constraint 'rw = [< `Rd | `Wr ]
  type ro = [ `Rd ]
  type wo = [ `Wr ]
  type rw = [ ro | wo ]

  external rw: int array -> rw t = "%identity" [@@noalloc] [@@inline]
  external ro: int array -> ro t = "%identity" [@@noalloc] [@@inline]
  external wo: int array -> wo t = "%identity" [@@noalloc] [@@inline]

  external protect_ro: [> ro ] t -> ro t = "%identity" [@@noalloc] [@@inline]
  external protect_wo: [> wo ] t -> wo t = "%identity" [@@noalloc] [@@inline]

  let get a i = Array.unsafe_get a i [@@inline]
  let set a i v = Array.unsafe_set a i v [@@inline]
  let length a = Array.length a [@@inline]
end

let pp ppf (arr:[> A.ro ] A.t) =
  for i = 0 to A.length arr - 1
  do Fmt.pf ppf "%02X" (A.get arr i) done

let add (outv:A.wo A.t) outv_offset (a:A.ro A.t) a_offset (b:A.ro A.t) b_offset =
  let u = ref 0 in
  for j = 0 to 30 do
    u := !u + (A.get a (a_offset + j)) + (A.get b (b_offset + j));
    A.set outv (outv_offset + j) (!u land 255);
    u := !u lsr 8;
  done;

  u := !u + (A.get a (a_offset + 31)) + (A.get b (b_offset + 31));
  A.set outv (outv_offset + 31) !u

let sub (outv:A.wo A.t) outv_offset (a:A.ro A.t) a_offset (b:A.ro A.t) b_offset =
  let u = ref 218 in
  for j = 0 to 30 do
    u := !u + (A.get a (a_offset + j))
          + 65280 - (A.get b (b_offset + j));
    A.set outv (outv_offset + j) (!u land 255);
    u := !u lsr 8;
  done;

  u := !u + (A.get a (a_offset + 31)) - (A.get b (b_offset + 31));
  A.set outv (outv_offset + 31) !u

let squeeze (a:A.rw A.t) a_offset =
  let u = ref 0 in
  for j = 0 to 30 do
    u := !u + (A.get a (a_offset + j));
    A.set a (a_offset + j) (!u land 255);
    u := !u lsr 8;
  done;

  u := !u + (A.get a (a_offset + 31));
  A.set a (a_offset + 31) (!u land 127);
  u := 19 * (!u lsr 7);

  for j = 0 to 30 do
    u := !u + (A.get a (a_offset + j));
    A.set a (a_offset + j) (!u land 255);
    u := !u lsr 8;
  done;

  u := !u + (A.get a (a_offset + 31));
  A.set a (a_offset + 31) !u

let freeze (a:A.rw A.t) a_offset =
  let a_orig = A.rw (Array.make 32 0) in

  (* XXX(dinosaure): Allocation here! *)

  for j = 0 to 31 do
    A.set a_orig j (A.get a (a_offset + j));
  done;

  add (A.protect_wo a) 0 (A.protect_ro a) 0 (A.ro minusp) 0;
  let negative = - (((A.get a (a_offset + 31)) lsr 7) land 1) in

  for j = 0 to 31 do
    A.set a (a_offset + j)
      (A.get a (a_offset + j)
       lxor (negative
              land ((A.get a_orig j) lxor (A.get a (a_offset + j)))));
  done

let mult (outv:A.rw A.t) outv_offset (a:A.ro A.t) a_offset (b:A.ro A.t) b_offset =
  for i = 0 to 31 do
    let u = ref 0 in

    for j = 0 to i do
      u := !u
        + ((A.get a (a_offset + j))
           * (A.get b (b_offset + i - j)));
    done;

    for j = i + 1 to 31 do
      u := !u + (38
                 * (A.get a (a_offset + j))
                 * (A.get b (b_offset + i + 32 - j)));
    done;

    A.set outv (outv_offset + i) !u;
  done;

  squeeze outv outv_offset

let mult21665 (outv:A.rw A.t) (a:A.ro A.t) =
  let u = ref 0 in

  for j = 0 to 30 do
    u := !u + (121665 * (A.get a j));
    A.set outv j (!u land 255);
    u := !u lsr 8;
  done;

  u := !u + (121665 * (A.get a 31));
  A.set outv 31 (!u land 127);
  u := 19 * (!u lsr 7);

  for j = 0 to 30 do
    u := !u + (A.get outv j);
    A.set outv j (!u land 255);
    u := !u lsr 8;
  done;

  u := !u + (A.get outv 31);
  A.set outv 31 !u

let square (outv:A.rw A.t) outv_offset (a:A.ro A.t) a_offset =
  for i = 0 to 31 do
    let u = ref 0 in
    let j = ref 0 in

    while !j < i - !j do
      u := !u
        + ((A.get a (a_offset + !j))
           * (A.get a (a_offset + i - !j)));
      incr j;
    done;

    j := i + 1;

    while !j < i + 32 - !j do
      u := !u + (38
                 * (A.get a (a_offset + !j))
                 * (A.get a (a_offset + i + 32 - !j)));
      incr j;
    done;

    u := !u * 2;

    if i land 1 = 0
    then begin
          u := !u
          + ((A.get a (a_offset + i / 2))
             * (A.get a (a_offset + i / 2)));
          u := !u + (38
                     * (A.get a (a_offset + i / 2 + 16))
                     * (A.get a (a_offset + i / 2 + 16)));
    end;

    A.set outv (outv_offset + i) !u;
  done;

  squeeze outv outv_offset

let select (p:A.wo A.t) (q:A.wo A.t) (r:A.ro A.t) (s:A.ro A.t) b =
  let bminus1 = b - 1 in

  for j = 0 to 63 do
    let t = bminus1 land ((A.get r j) lxor (A.get s j)) in
    A.set p j ((A.get s j) lxor t);
    A.set q j ((A.get r j) lxor t);
  done

let main_loop ctx (work:A.rw A.t) (e:A.ro A.t) =
  let xzm1   = A.rw ctx.xzm1  in
  let xzm    = A.rw ctx.xzm   in
  let xzmb   = A.rw ctx.xzmb  in
  let xzm1b  = A.rw ctx.xzm1b in
  let xznb   = A.rw ctx.xznb  in
  let xzn1b  = A.rw ctx.xzn1b in
  let a0     = A.rw ctx.a0    in
  let a1     = A.rw ctx.a1    in
  let b0     = A.rw ctx.b0    in
  let b1     = A.rw ctx.b1    in
  let c1     = A.rw ctx.c1    in
  let r      = A.rw ctx.r     in
  let s      = A.rw ctx.s     in
  let t      = A.rw ctx.t     in
  let u      = A.rw ctx.u     in

  for j = 0 to 31 do
    A.set xzm1 j (A.get work j);
  done;

  A.set xzm1 32 1;
  A.set xzm 0 1;

  for pos = 254 downto 0 do
    let b =
      ((A.get e (pos / 8)) land 0xFF)
      lsr (pos land 7) in
    let b = b land 1 in
    select      (A.protect_wo xzmb)     (A.protect_wo xzm1b)   (A.protect_ro xzm)     (A.protect_ro xzm1)    b;
    add         (A.protect_wo a0)       0                      (A.protect_ro xzmb)    0                      (A.protect_ro xzmb)     32;
    sub         (A.protect_wo a0)       32                     (A.protect_ro xzmb)    0                      (A.protect_ro xzmb)     32;
    add         (A.protect_wo a1)       0                      (A.protect_ro xzm1b)   0                      (A.protect_ro xzm1b)    32;
    sub         (A.protect_wo a1)       32                     (A.protect_ro xzm1b)   0                      (A.protect_ro xzm1b)    32;
    square      b0                      0                      (A.protect_ro a0)      0;
    square      b0                      32                     (A.protect_ro a0)      32;
    mult        b1                      0                      (A.protect_ro a1)      0                      (A.protect_ro a0)       32;
    mult        b1                      32                     (A.protect_ro a1)      32                     (A.protect_ro a0)       0;
    add         (A.protect_wo c1)       0                      (A.protect_ro b1)      0                      (A.protect_ro b1)       32;
    sub         (A.protect_wo c1)       32                     (A.protect_ro b1)      0                      (A.protect_ro b1)       32;
    square      r                       0                      (A.protect_ro c1)      32;
    sub         (A.protect_wo s)        0                      (A.protect_ro b0)      0                      (A.protect_ro b0)       32;
    mult21665   t                       (A.protect_ro s);
    add         (A.protect_wo u)        0                      (A.protect_ro t)       0                      (A.protect_ro b0)       0;
    mult        xznb                    0                      (A.protect_ro b0)      0                      (A.protect_ro b0)       32;
    mult        xznb                    32                     (A.protect_ro s)       0                      (A.protect_ro u)        0;
    square      xzn1b                   0                      (A.protect_ro c1)      0;
    mult        xzn1b                   32                     (A.protect_ro r)       0                      (A.protect_ro work)     0;
    select      (A.protect_wo xzm)      (A.protect_wo xzm1)    (A.protect_ro xznb)    (A.protect_ro xzn1b)   b;
  done;

  for j = 0 to 63 do
    A.set work j (A.get xzm j)
  done;

  reset_ctx_main_loop ctx

let recip ctx (outv:A.rw A.t) outv_offset (z:A.ro A.t) z_offset =
  let z2       = A.rw ctx.z2       in
  let z9       = A.rw ctx.z9       in
  let z11      = A.rw ctx.z11      in
  let z2_5_0   = A.rw ctx.z2_5_0   in
  let z2_10_0  = A.rw ctx.z2_10_0  in
  let z2_20_0  = A.rw ctx.z2_20_0  in
  let z2_50_0  = A.rw ctx.z2_50_0  in
  let z2_100_0 = A.rw ctx.z2_100_0 in
  let t0       = A.rw ctx.t0       in
  let t1       = A.rw ctx.t1       in

  square   z2       0           z                       z_offset;                                  (* 2 *)
  square   t1       0           (A.protect_ro z2)       0;                                         (* 4 *)
  square   t0       0           (A.protect_ro t1)       0;                                         (* 8 *)
  mult     z9       0           (A.protect_ro t0)       0         z                      z_offset; (* 9 *)
  mult     z11      0           (A.protect_ro z9)       0         (A.protect_ro z2)      0;        (* 11 *)
  square   t0       0           (A.protect_ro z11)      0;                                         (* 22 *)
  mult     z2_5_0   0           (A.protect_ro t0)       0         (A.protect_ro z9)      0;        (* 2^5 - 2^0 = 31 *)
  square   t0       0           (A.protect_ro z2_5_0)   0;                                         (* 2^6 - 2^1 *)
  square   t1       0           (A.protect_ro t0)       0;                                         (* 2^7 - 2^2 *)
  square   t0       0           (A.protect_ro t1)       0;                                         (* 2^8 - 2^3 *)
  square   t1       0           (A.protect_ro t0)       0;                                         (* 2^9 - 2^4 *)
  square   t0       0           (A.protect_ro t1)       0;                                         (* 2^10 - 2^5 *)
  mult     z2_10_0  0           (A.protect_ro t0)       0         (A.protect_ro z2_5_0)  0;        (* 2^10 - 2^0 *)
  square   t0       0           (A.protect_ro z2_10_0)  0;                                         (* 2^11 - 2^1 *)
  square   t1       0           (A.protect_ro t0)       0;                                         (* 2^12 - 2^2 *)

  (* 2^20 - 2^10 *)
  for i = 1 to 4 do
    square t0       0           (A.protect_ro t1)       0;
    square t1       0           (A.protect_ro t0)       0;
  done;

  mult     z2_20_0  0           (A.protect_ro t1)       0         (A.protect_ro z2_10_0) 0;        (* 2^20 - 2^0 *)
  square   t0       0           (A.protect_ro z2_20_0)  0;                                         (* 2^21 - 2^1 *)
  square   t1       0           (A.protect_ro t0)       0;                                         (* 2^22 - 2^2 *)

  (* 2^40 - 2^40 *)
  for i = 1 to 9 do
    square t0       0           (A.protect_ro t1)       0;
    square t1       0           (A.protect_ro t0)       0;
  done;

  mult     t0       0           (A.protect_ro t1)       0         (A.protect_ro z2_20_0) 0;        (* 2^40 - 2^0 *)
  square   t1       0           (A.protect_ro t0)       0;                                         (* 2^41 - 2^1 *)
  square   t0       0           (A.protect_ro t1)       0;                                         (* 2^42 - 2^2 *)

  (* 2^50 - 2^10 *)
  for i = 1 to 4 do
    square t1       0           (A.protect_ro t0)       0;
    square t0       0           (A.protect_ro t1)       0;
  done;

  mult     z2_50_0  0           (A.protect_ro t0)       0         (A.protect_ro z2_10_0) 0;        (* 2^50 - 2^0 *)
  square   t0       0           (A.protect_ro z2_50_0)  0;                                         (* 2^51 - 2^1 *)
  square   t1       0           (A.protect_ro t0)       0;                                         (* 2^52 - 2^2 *)

  (* 2^100 - 2^50 *)
  for i = 1 to 24 do
    square t0       0           (A.protect_ro t1)       0;
    square t1       0           (A.protect_ro t0)       0;
  done;

  mult     z2_100_0 0           (A.protect_ro t1)       0         (A.protect_ro z2_50_0) 0;        (* 2^100 - 2^0 *)
  square   t1       0           (A.protect_ro z2_100_0) 0;                                         (* 2^101 - 2^1 *)
  square   t0       0           (A.protect_ro t1)       0;                                         (* 2^102 - 2^2 *)

  (* 2^200 - 2^100 *)
  for i = 1 to 49 do
    square t1       0           (A.protect_ro t0)       0;
    square t0       0           (A.protect_ro t1)       0;
  done;

  mult     t1       0           (A.protect_ro t0)       0         (A.protect_ro z2_100_0) 0;       (* 2^200 - 2^0 *)
  square   t0       0           (A.protect_ro t1)       0;                                         (* 2^201 - 2^1 *)
  square   t1       0           (A.protect_ro t0)       0;                                         (* 2^202 - 2^2 *)

  (* 2^250 - 2^50 *)
  for i = 1 to 24 do
    square t0       0           (A.protect_ro t1)       0;
    square t1       0           (A.protect_ro t0)       0;
  done;

  mult     t0       0           (A.protect_ro t1)       0         (A.protect_ro z2_50_0) 0;        (* 2^250 - 2^0 *)
  square   t1       0           (A.protect_ro t0)       0;                                         (* 2^251 - 2^1 *)
  square   t0       0           (A.protect_ro t1)       0;                                         (* 2^252 - 2^2 *)
  square   t1       0           (A.protect_ro t0)       0;                                         (* 2^253 - 2^3 *)
  square   t0       0           (A.protect_ro t1)       0;                                         (* 2^254 - 2^4 *)
  square   t1       0           (A.protect_ro t0)       0;                                         (* 2^255 - 2^5 *)
  mult     outv     outv_offset (A.protect_ro t1)       0         (A.protect_ro z11)     0;        (* 2^255 - 21 *)

  reset_ctx_recip ctx

let curve25519 ctx (q:A.wo A.t) (n:A.ro A.t) (p:A.ro A.t) =
  let work = A.rw (Array.make 96 0) in
  let e = A.rw (Array.make 32 0) in

  for i = 0 to 31 do
    A.set e i (A.get n i);
  done;

  A.set e 0  ((A.get e 0 ) land 248);
  A.set e 31 ((A.get e 31) land 127);
  A.set e 31 ((A.get e 31) lor  64 );

  for i = 0 to 31 do
    A.set work i (A.get p i land 0xFF);
  done;

  main_loop   ctx    work   (A.protect_ro e);
  recip       ctx    work   32                  (A.protect_ro work)   32;
  mult        work   64     (A.protect_ro work) 0                     (A.protect_ro work) 32;
  freeze      work   64;

  for i = 0 to 31 do
    A.set q i (A.get work (64 + i)) done;

  ()

let curve25519_base q n =
  let basevp = basev in
  curve25519 q n (A.ro basevp)

type _ key = int array
type public = P
type secret = S
type shared = SS

external identity: 'a -> 'a = "%identity"
let (<.>) f g = fun x -> f (g x)

let secret_key_of_string
  : string -> secret key
  = fun x ->
    if String.length x <> 32
    then Fmt.invalid_arg "secret_key_of_string: key should consist of 32 bytes";
    Array.init 32 (Char.code <.> String.get x)

let secret_key_of_int_array
  : int array -> secret key
  = fun x ->
    if Array.length x <> 32 || Array.exists (fun x -> x > 0xFF) x
    then Fmt.invalid_arg "public_key_of_int_array: key should consist of 32 bytes";
    identity x

let null = String.make 32 '\x00'

let public_key_of_string
  : string -> public key
  = fun x ->
    if String.length x <> 32
    then Fmt.invalid_arg "public_key_of_string: key should consist of 32 bytes";
    if String.equal x null
    then Fmt.invalid_arg "public_key_of_string: null public key";
    Array.init 32 (Char.code <.> String.get x)

let public_key_of_int_array
  : int array -> public key
  = fun x ->
    if Array.length x <> 32 || Array.exists (fun x -> x > 0xFF) x
    then Fmt.invalid_arg "public_key_of_int_array: key should consist of 32 bytes";
    if Array.for_all ((=) 0) x
    then Fmt.invalid_arg "public_key_of_int_array: null public key";
    identity x

let string_of_key
  : _ key -> string
  = fun x ->
    (* assert (Array.length x = 32); *)
    String.init 32 (Char.chr <.> Array.get x)

let ecdh_base
  : ctx:ctx -> out:int array -> secret:secret key -> unit
  = fun ~ctx ~out ~secret -> curve25519_base ctx (A.wo out) (A.ro secret)
let ecdh
  : ctx:ctx -> out:int array -> secret:secret key -> public:public key -> unit
  = fun ~ctx ~out ~secret ~public -> curve25519 ctx (A.wo out) (A.ro secret) (A.ro public)

let base: public key = basev

let public_of_secret
  : ctx:ctx -> secret key -> public key
  = fun ~ctx secret ->
  let r = Array.make 32 0 in
    ecdh_base ctx r secret
  ; r

let shared
  : ctx:ctx -> secret:secret key -> public:public key -> shared key
  = fun ~ctx ~secret ~public ->
  let r = Array.make 32 0 in
    ecdh ctx r secret public
  ; r

let public_key_of_shared x = identity x [@@noalloc] [@@inline]
let secret_key_of_shared x = identity x [@@noalloc] [@@inline]

let pp_public_key: public key Fmt.t = fun ppf key -> pp ppf (A.ro key)
let pp_shared_key: shared key Fmt.t = fun ppf key -> pp ppf (A.ro key)

let equal_key: _ key -> _ key -> bool
  = fun a b ->
    (* assert (Array.length a = 32 && Array.length b = 32); *)
    let rs = ref 0 in
    for i = 0 to 31
    do rs := !rs lor ((Array.get a i) lxor (Array.get b i)) done;
    !rs = 0
