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
      |> fun u -> aux2 u i
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
               + 38 * (Array.get a (a_offset + i))
               * (Array.get a (a_offset + i + 32 - i))
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
