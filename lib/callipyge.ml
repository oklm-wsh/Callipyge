module type Array =
  sig
    type t

    val get  : t -> int -> int
    val set  : t -> int -> int -> unit
    val sub  : t -> int -> int -> t
    val init : int -> (int -> int) -> t
    val make : int -> int -> t
  end

module Make (X : Array) =
  struct
    let basev =
      X.init 32 (function 0 -> 9 | _ -> 0)

    let minusp =
      X.init 32 (function 0 -> 19 | 31 -> 128 | _ -> 0)

    let add outv outv_offset a a_offset b b_offset =
      let rec aux u = function
        | j when j < 31->
          u
          |> (+) (X.get a (a_offset + j))
          |> (+) (X.get b (b_offset + j))
          |> fun u ->
            X.set outv (outv_offset + j) (u land 255);
            aux (u lsr 8) (j + 1)
        | _ -> u
      in
      aux 0 0
      |> (+) (X.get a (a_offset + 31))
      |> (+) (X.get b (b_offset + 31))
      |> X.set outv (outv_offset + 31)

    let add outv outv_offset a a_offset b b_offset =
      let u = ref 0 in
      for j = 0 to 30 do
        u := !u + (X.get a (a_offset + j)) + (X.get b (b_offset + j));
        X.set outv (outv_offset + j) (!u land 255);
        u := !u lsr 8;
      done;

      u := !u + (X.get a (a_offset + 31)) + (X.get b (b_offset + 31));
      X.set outv (outv_offset + 31) !u

    let sub outv outv_offset a a_offset b b_offset =
      let rec aux u = function
        | j when j < 31 ->
          u
          |> (+) (X.get a (a_offset + j))
          |> (+) 65280
          |> (-) (X.get b (b_offset + j))
          |> fun u ->
            X.set outv (outv_offset + j) (u land 255);
            aux (u lsr 8) (j + 1)
        | _ -> u
      in
      aux 218 0
      |> (+) (X.get a (a_offset + 31))
      |> (+) (X.get b (b_offset + 31))
      |> X.set outv (outv_offset + 31)

    let sub outv outv_offset a a_offset b b_offset =
      let u = ref 218 in
      for j = 0 to 30 do
        u := !u + (X.get a (a_offset + j))
             + 65280 - (X.get b (b_offset + j));
        X.set outv (outv_offset + j) (!u land 255);
        u := !u lsr 8;
      done;

      u := !u + (X.get a (a_offset + 31)) - (X.get b (b_offset + 31));
      X.set outv (outv_offset + 31) !u

    let squeeze a a_offset =
      let rec aux j u = match j with
        | j when j < 31 ->
          u
          |> (+) (X.get a (a_offset + j))
          |> fun u -> let () = X.set a (a_offset + 31) (u land 255) in u
          |> fun u -> aux (j + 1) (u lsr 8)
        | _ -> u
      in
      aux 0 0
      |> (+) (X.get a (a_offset + 31))
      |> fun u -> let () = X.set a (a_offset + 31) (u land 127) in u
      |> fun u -> 19 * (u lsr 7)
      |> aux 0
      |> (+) (X.get a (a_offset + 31))
      |> X.set a (a_offset + 31)

    let squeeze a a_offset =
      let u = ref 0 in
      for j = 0 to 30 do
        u := !u + (X.get a (a_offset + j));
        X.set a (a_offset + j) (!u land 255);
        u := !u lsr 8;
      done;

      u := !u + (X.get a (a_offset + 31));
      X.set a (a_offset + 31) (!u land 127);
      u := 19 * (!u lsr 7);

      for j = 0 to 30 do
        u := !u + (X.get a (a_offset + j));
        X.set a (a_offset + j) (!u land 255);
        u := !u lsr 8;
      done;

      u := !u + (X.get a (a_offset + 31));
      X.set a (a_offset + 31) !u

    let freeze a a_offset =
      let a_orig = X.sub a a_offset 32 in

      add a 0 a 0 minusp 0
      |> fun () -> - (((X.get a a_offset + 31) lsr 7) land 1)
      |> fun negative ->
        for j = 0 to 31 do
          X.get a (a_offset + j)
          lxor (negative
                land (X.get a_orig j
                      lxor X.get a (a_offset + j)))
          |> X.set a (a_offset + j)
        done

    let freeze a a_offset =
      let a_orig = X.sub a a_offset 32 in

      for j = 0 to 31 do
        X.set a_orig j (X.get a (a_offset + j));
      done;

      let minuspp = minusp in
      add a 0 a 0 minuspp 0;
      let negative = - (((X.get a (a_offset + 31)) lsr 7) land 1) in

      for j = 0 to 31 do
        X.set a (a_offset + j)
          (X.get a (a_offset + j)
           lxor negative
           land ((X.get a_orig j) lxor (X.get a (a_offset + j))));
      done

    let mult outv outv_offset a a_offset b b_offset =
      let rec aux = function
        | i when i < 32 ->
          let rec aux1 u = function
            | j when j <= i ->
              (X.get a (a_offset + j)) * (X.get b (b_offset + i - j))
              |> (+) u
              |> fun u -> aux1 u (j + 1)
            | _ -> u
          in
          let rec aux2 u = function
            | j when j < 32 ->
              38
              |> ( * ) (X.get a (a_offset + j))
              |> ( * ) (X.get b (b_offset + i + 32 - j))
              |> (+) u
            | _ -> u
          in
          aux1 0 0
          |> fun u -> aux2 u (i + 1)
          |> X.set outv (outv_offset + i)
          |> fun () -> aux (i + 1)
        | _ -> squeeze outv outv_offset
      in aux 0

    let mult outv outv_offset a a_offset b b_offset =
      for i = 0 to 31 do
        let u = ref 0 in

        for j = 0 to i do
          u := !u
            + (X.get a (a_offset + j))
            * (X.get b (b_offset + i - j));
        done;

        for j = i + 1 to 31 do
          u := !u + 38
            * (X.get a (a_offset + j))
            * (X.get b (b_offset + i + 32 - j));
        done;

        X.set outv (outv_offset + i) !u;
      done;

      squeeze outv outv_offset

    let mult21665 outv a =
      let rec aux1 u = function
        | j when j < 31 ->
          u + 121665 * (X.get a j)
          |> fun u ->
            X.set outv j (u land 255);
            aux1 (u lsr 8) (j + 1)
        | _ -> u
      in
      let rec aux2 u = function
        | j when j < 31 ->
          u + (X.get outv j)
          |> fun u ->
            X.set outv j (u land 255);
            aux2 (u lsr 8) (j + 1)
        | _ -> u
      in
      aux1 0 0
      |> (+) 121665
      |> ( * ) (X.get a 31)
      |> fun u -> X.set outv 31 (u land 127); 19 * (u lsr 7)
      |> aux2 0
      |> (+) (X.get outv 31)
      |> X.set outv 31

    let mult21665 outv a =
      let u = ref 0 in

      for j = 0 to 30 do
        u := !u + 121665 * (X.get a j);
        X.set outv j (!u land 255);
        u := !u lsr 8;
      done;

      u := !u + 121665 * (X.get a 31);
      X.set outv 31 (!u land 127);
      u := 19 * (!u lsr 7);

      for j = 0 to 30 do
        u := !u + (X.get outv j);
        X.set outv j (!u land 255);
        u := !u lsr 8;
      done;

      u := !u + (X.get outv 31);
      X.set outv 31 !u

    let square outv outv_offset a a_offset =
      let rec aux = function
        | i when i < 32 ->
          let rec aux1 u = function
            | j when j < i - j ->
              (X.get a (a_offset + j))
              |> ( * ) (X.get a (a_offset + i - j))
              |> (+) u
              |> fun u -> aux1 u (j + 1)
            | _ -> u
          in
          let rec aux2 u = function
            | j when j < i + 32 - j ->
              38
              |> ( * ) (X.get a (a_offset + j))
              |> ( * ) (X.get a (a_offset + i + 32 - j))
              |> (+) u
              |> fun u -> aux2 u (j + 1)
            | _ -> u
          in
          aux1 0 0
          |> fun u -> aux2 u (i + 1)
          |> ( * ) 2
          |> fun u ->
             (if i land 1 = 0
              then (X.get a (a_offset + i / 2))
                   * (X.get a (a_offset + i / 2))
                   + 38 * (X.get a (a_offset + i / 2 + 16))
                   * (X.get a (a_offset + i / 2 + 16))
                   + u
              else u)
          |> fun u ->
            X.set outv (outv_offset + i) u;
            aux (i + 1)
        | _ -> squeeze outv outv_offset
      in aux 0

    let square outv outv_offset a a_offset =
      for i = 0 to 31 do
        let u = ref 0 in
        let j = ref 0 in

        while !j < i - !j do
          u := !u
            + (X.get a (a_offset + !j))
            * (X.get a (a_offset + i - !j));
          incr j;
        done;

        u := !u * 2;

        if i land 1 = 0
        then begin
             u := !u
              + (X.get a (a_offset + i / 2))
              * (X.get a (a_offset + i / 2));
             u := !u + 38
              * (X.get a (a_offset + i / 2 + 16))
              * (X.get a (a_offset + i / 2 + 16));
        end;

        X.set outv (outv_offset + i) !u;
      done;

      squeeze outv outv_offset

    let select p q r s b =
      let bminus1 = b - 1 in
      for j = 0 to 63 do
        let t = bminus1 land ((X.get r j) lxor (X.get s j)) in
        X.set p j ((X.get s j) lxor t);
        X.set q j ((X.get r j) lxor t);
      done

    let main_loop work e =
      let xzm1 = X.init 64
        (function
         | j when j < 32 -> X.get work j
         | 32 -> 1
         | _ -> 0) in
      let xzm = X.init 64
        (function
         | 0 -> 1
         | _ -> 0) in
      let xzmb   = X.make 64 0 in
      let xzm1b  = X.make 64 0 in
      let xznb   = X.make 64 0 in
      let xzn1b  = X.make 64 0 in
      let a0     = X.make 64 0 in
      let a1     = X.make 64 0 in
      let b0     = X.make 64 0 in
      let b1     = X.make 64 0 in
      let c1     = X.make 64 0 in
      let r      = X.make 32 0 in
      let s      = X.make 32 0 in
      let t      = X.make 32 0 in
      let u      = X.make 32 0 in

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
          ((X.get e (pos / 8)) land 0xFF)
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
        X.set work j (X.get xzm j)
      done

    let recip outv outv_offset z z_offset =
      let z2       = X.make 32 0 in
      let z9       = X.make 32 0 in
      let z11      = X.make 32 0 in
      let z2_5_0   = X.make 32 0 in
      let z2_10_0  = X.make 32 0 in
      let z2_20_0  = X.make 32 0 in
      let z2_50_0  = X.make 32 0 in
      let z2_100_0 = X.make 32 0 in
      let t0       = X.make 32 0 in
      let t1       = X.make 32 0 in

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

    let curve25519 q n p =
      (* TODO: blit *)
      let work = X.init 96
        (function i when i < 32 -> X.get p i land 0xFF | _ -> 0) in
      let e = X.init 32 (X.get n) in

      X.set e 0  ((X.get e 0 ) land 248);
      X.set e 31 ((X.get e 31) land 127);
      X.set e 31 ((X.get e 31) lor  64 );

      main_loop   work   e;
      recip       work   32    work   32;
      mult        work   64    work   0     work   32;
      freeze      work   64;

      for i = 0 to 31 do
        X.set q i (X.get work (64 + i)) done;

      0

    let curve25519_base q n =
      let basevp = basev in
      curve25519 q n basevp
  end
