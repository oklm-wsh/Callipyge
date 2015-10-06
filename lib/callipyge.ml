let basev =
  [| 9; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
     0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; |]

let minusp =
  [| 19; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
      0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 128; |]

let add outv outv_offset a a_offset b b_offset =
  let rec aux u = function
    | j when j < 31->
      u
      |> (+) (Array.get a (a_offset + j))
      |> (+) (Array.get b (b_offset + j))
      |> fun u ->
        Array.set outv (outv_offset + j) (u land 255);
        aux (u lsr 8) (j + 1)
    | _ -> u
  in
  aux 0 0
  |> (+) (Array.get a (a_offset + 31))
  |> (+) (Array.get b (b_offset + 31))
  |> Array.set outv (outv_offset + 31)

let sub outv outv_offset a a_offset b b_offset =
  let rec aux u = function
    | j when j < 31 ->
      u
      |> (+) (Array.get a (a_offset + j))
      |> (+) 65280
      |> (-) (Array.get b (b_offset + j))
      |> fun u ->
        Array.set outv (outv_offset + j) (u land 255);
        aux (u lsr 8) (j + 1)
    | _ -> u
  in
  aux 218 0
  |> (+) (Array.get a (a_offset + 31))
  |> (+) (Array.get b (b_offset + 31))
  |> Array.set outv (outv_offset + 31)

let squeeze a a_offset =
  let rec aux j u = match j with
    | j when j < 31 ->
      u
      |> (+) (Array.get a (a_offset + j))
      |> fun u -> let () = Array.set a (a_offset + 31) (u land 255) in u
      |> fun u -> aux (j + 1) (u lsr 8)
    | _ -> u
  in
  aux 0 0
  |> (+) (Array.get a (a_offset + 31))
  |> fun u -> let () = Array.set a (a_offset + 31) (u land 127) in u
  |> fun u -> 19 * (u lsr 7)
  |> aux 0
  |> (+) (Array.get a (a_offset + 31))
  |> Array.set a (a_offset + 31)

let freeze a a_offset =
  let a_orig = Array.sub a a_offset 32 in

  add a 0 a 0 minusp 0
  |> fun () -> - ((Array.get a a_offset + 31) lsr 7) land 1
  |> fun negative ->
    for j = 0 to 31 do
      Array.get a (a_offset + j)
      lxor (negative
            land (Array.get a_orig j
                  lxor Array.get a (a_offset + j)))
      |> Array.set a (a_offset + j)
    done

let mult outv outv_offset a a_offset b b_offset =
  let rec aux = function
    | i when i < 32 ->
      let rec aux1 u = function
        | j when j <= i ->
          (Array.get a (a_offset + j)) * (Array.get b (b_offset + i - j))
          |> (+) u
          |> fun u -> aux1 u (j + 1)
        | _ -> u
      in
      let rec aux2 u = function
        | j when j < 32 ->
          38
          |> ( * ) (Array.get a (a_offset + j))
          |> ( * ) (Array.get b (b_offset + i + 32 - j))
          |> (+) u
        | _ -> u
      in
      aux1 0 0
      |> fun u -> aux2 u (i + 1)
      |> Array.set outv (outv_offset + i)
      |> fun () -> aux (i + 1)
    | _ -> squeeze outv outv_offset
  in aux 0

let mult21665 outv a =
  let rec aux1 u = function
    | j when j < 31 ->
      u + 121665 * (Array.get a j)
      |> fun u ->
        Array.set outv j (u land 255);
        aux1 (u lsr 8) (j + 1)
    | _ -> u
  in
  let rec aux2 u = function
    | j when j < 31 ->
      u + (Array.get outv j)
      |> fun u ->
        Array.set outv j (u land 255);
        aux2 (u lsr 8) (j + 1)
    | _ -> u
  in
  aux1 0 0
  |> (+) 121665
  |> ( * ) (Array.get a 31)
  |> fun u -> Array.set outv 31 (u land 127); 19 * (u lsr 7)
  |> aux2 0
  |> (+) (Array.get outv 31)
  |> Array.set outv 31

let square outv outv_offset a a_offset =
  let rec aux = function
    | i when i < 32 ->
      let rec aux1 u = function
        | j when j < i - j ->
          (Array.get a (a_offset + j))
          |> ( * ) (Array.get a (a_offset + i - j))
          |> (+) u
          |> fun u -> aux1 u (j + 1)
        | _ -> u
      in
      let rec aux2 u = function
        | j when j < i + 32 - j ->
          38
          |> ( * ) (Array.get a (a_offset + j))
          |> ( * ) (Array.get a (a_offset + i + 32 - j))
          |> (+) u
          |> fun u -> aux2 u (j + 1)
        | _ -> u
      in
      aux1 0 0
      |> fun u -> aux2 u (i + 1)
      |> ( * ) 2
      |> fun u ->
         (if i land 1 = 0
          then (Array.get a (a_offset + i / 2))
               * (Array.get a (a_offset + i / 2))
               + 38 * (Array.get a (a_offset + i / 2 + 16))
               * (Array.get a (a_offset + i / 2 + 16))
               + u
          else u)
      |> fun u ->
        Array.set outv (outv_offset + i) u;
        aux (i + 1)
    | _ -> squeeze outv outv_offset
  in aux 0

let select p q r s b =
  let bminus1 = b - 1 in
  for j = 0 to 63 do
    let t = bminus1 land ((Array.get r j) lxor (Array.get s j)) in
    Array.set p j ((Array.get s j) lxor t);
    Array.set q j ((Array.get r j) lxor t);
  done

let main_loop work e =
  let xzm1 = Array.init 64
    (function
     | j when j < 32 -> Array.get work j
     | 32 -> 1
     | _ -> 0) in
  let xzm = Array.init 64
    (function
     | 0 -> 1
     | _ -> 0) in
  let xzmb   = Array.make 64 0 in
  let xzm1b  = Array.make 64 0 in
  let xznb   = Array.make 64 0 in
  let xzn1b  = Array.make 64 0 in
  let a0     = Array.make 64 0 in
  let a1     = Array.make 64 0 in
  let b0     = Array.make 64 0 in
  let b1     = Array.make 64 0 in
  let c1     = Array.make 64 0 in
  let r      = Array.make 32 0 in
  let s      = Array.make 32 0 in
  let t      = Array.make 32 0 in
  let u      = Array.make 32 0 in

  let xzmbp  = xzmb in
  let a0p    = a0 in
  let xzm1bp = xzm1b in
  let a1p    = a1 in
  let b0p    = b0 in
  let b1p    = b1 in
  let c1p    = c1 in
  let xznbp  = xznb in
  let up     = u in
  let xzn1bp = xzn1b in
  let workp  = work in
  let sp     = s in
  let rp     = r in

  for pos = 254 downto 0 do
    let b =
      ((Bytes.get e (pos / 8) |> Char.code) land 0xFF)
      lsr (pos land 7)
      land 1 in
    select      xzmb     xzm1b   xzm     xzm1    b;
    add         a0       0       xzmb    0       xzmbp    32;
    sub         a0p      32      xzmb    0       xzmbp    32;
    add         a1       0       xzm1b   0       xzm1bp   32;
    sub         a1p      32      xzm1b   0       xzm1bp   32;
    square      b0p      0       a0p     0;
    square      b0p      32      a0p     32;
    mult        b1p      0       a1p     0       a0p      32;
    mult        b1p      32      a1p     32      a0p      0;
    add         c1       0       b1      0       b1p      32;
    sub         c1p      32      b1      0       b1p      32;
    square      rp       0       c1p     32;
    sub         sp       0       b0      0       b0p      32;
    mult21665   t        s;
    add         u        0       t       0       b0p      0;
    mult        xznbp    0       b0p     0       b0p      32;
    mult        xznbp    32      sp      0       up       0;
    square      xzn1bp   0       c1p     0;
    mult        xzn1bp   32      rp      0       workp    0;
    select      xzm      xzm1    xznb    xzn1b   b;
  done;

  for j = 0 to 64 - 1 do
    Array.set work j (Array.get xzm j)
  done

let recip outv outv_offset z z_offset =
  let z2       = Array.make 32 0 in
  let z9       = Array.make 32 0 in
  let z11      = Array.make 32 0 in
  let z2_5_0   = Array.make 32 0 in
  let z2_10_0  = Array.make 32 0 in
  let z2_20_0  = Array.make 32 0 in
  let z2_50_0  = Array.make 32 0 in
  let z2_100_0 = Array.make 32 0 in
  let t0       = Array.make 32 0 in
  let t1       = Array.make 32 0 in

  let z2p = z2 in
  square   z2p      0           z        z_offset; (* 2 *)
  square   t1       0           z2       0;        (* 4 *)
  square   t0       0           t1       0;        (* 8 *)

  let z9p = z9 in
  let t0p = t0 in
  mult     z9p      0           t0p      0         z       z_offset; (* 9 *)
  mult     z11      0           z9       0         z2      0;        (* 11 *)
  square   t0       0           z11      0;        (* 22 *)
  mult     z2_5_0   0           t0       0         z9      0; (* 2^5 - 2^0 = 31 *)
  square   t0       0           z2_5_0   0;        (* 2^6 - 2^1 *)
  square   t1       0           t0       0;        (* 2^7 - 2^2 *)
  square   t0       0           t1       0;        (* 2^8 - 2^3 *)
  square   t1       0           t0       0;        (* 2^9 - 2^4 *)
  square   t0       0           t1       0;        (* 2^10 - 2^5 *)
  mult     z2_10_0  0           t0       0         z2_5_0  0; (* 2^10 - 2^0 *)
  square   t0       0           z2_10_0  0;        (* 2^11 - 2^1 *)
  square   t1       0           t0       0;        (* 2^12 - 2^2 *)

  (* 2^20 - 2^10 *)
  for i = 1 to 4 do
    square t0       0           t1       0;
    square t1       0           t0       0;
  done;

  mult     z2_20_0  0           t1       0         z2_10_0 0; (* 2^20 - 2^0 *)
  square   t0       0           z2_20_0  0; (* 2^21 - 2^1 *)
  square   t1       0           t0       0; (* 2^22 - 2^2 *)

  (* 2^40 - 2^40 *)
  for i = 1 to 9 do
    square t0       0           t1      0;
    square t1       0           t0      0;
  done;

  mult     t0       0           t1       0         z2_20_0 0; (* 2^40 - 2^0 *)
  square   t1       0           t0       0; (* 2^41 - 2^1 *)
  square   t0       0           t1       0; (* 2^42 - 2^2 *)

  (* 2^50 - 2^10 *)
  for i = 1 to 4 do
    square t1       0           t0      0;
    square t0       0           t1      0;
  done;

  mult     z2_50_0  0           t0       0         z2_10_0 0; (* 2^50 - 2^0 *)
  square   t0       0           z2_50_0  0; (* 2^51 - 2^1 *)
  square   t1       0           t0       0; (* 2^52 - 2^2 *)

  (* 2^100 - 2^50 *)
  for i = 1 to 24 do
    square t0       0           t1      0;
    square t1       0           t0      0;
  done;

  mult     z2_100_0 0           t1       0         z2_50_0 0; (* 2^100 - 2^0 *)
  square   t1       0           z2_100_0 0;        (* 2^101 - 2^1 *)
  square            t0          0        t1        0; (* 2^102 - 2^2 *)

  (* 2^200 - 2^100 *)
  for i = 1 to 49 do
    square t1       0           t0      0;
    square t0       0           t1      0;
  done;

  mult     t1       0           t0       0         z2_100_0 0; (* 2^200 - 2^0 *)
  square   t0       0           t1       0; (* 2^201 - 2^1 *)
  square   t1       0           t0       0; (* 2^202 - 2^2 *)

  (* 2^250 - 2^50 *)
  for i = 1 to 24 do
    square t0       0           t1      0;
    square t1       0           t0      0;
  done;

  mult     t0       0           t1       0         z2_50_0 0; (* 2^250 - 2^0 *)
  square   t1       0           t0       0; (* 2^251 - 2^1 *)
  square   t0       0           t1       0; (* 2^252 - 2^2 *)
  square   t1       0           t0       0; (* 2^253 - 2^3 *)
  square   t0       0           t1       0; (* 2^254 - 2^4 *)
  square   t1       0           t0       0; (* 2^255 - 2^5 *)

  let      t1p      =           t1       in
  let      z11p     =           z11      in

  mult     outv     outv_offset t1p      0         z11p    0 (* 2^255 - 21 *)

let crypto_scalar_mult q n p =
  let work = Array.init 96
    (function i when i < 32 -> Array.get p i land 0xFF | _ -> 0) in
  let e = Bytes.init 32 (Bytes.get n) in

  Bytes.set e 0  ((Bytes.get e 0  |> Char.code) land 248 |> Char.unsafe_chr);
  Bytes.set e 31 ((Bytes.get e 31 |> Char.code) land 127 |> Char.unsafe_chr);
  Bytes.set e 31 ((Bytes.get e 31 |> Char.code) lor  64  |> Char.unsafe_chr);

  main_loop   work   e;
  recip       work   32    work   32;
  mult        work   64    work   0     work   32;
  freeze      work   64;

  for i = 0 to 31 do
    Bytes.set q i (Array.get work (64 + i) |> Char.unsafe_chr) done;

  0

let crypto_scalar_mult_base q n =
  let basevp = basev in
  crypto_scalar_mult q n basevp
