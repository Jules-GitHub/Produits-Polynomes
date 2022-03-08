type polynome = { deg : int; coeff : float list };;

let print_list liste =
  print_char '[';
  let rec aux l = match l with
  | h::[] -> print_float h; print_char ']'
  | h::t  -> print_float h; print_char ';'; aux t
  | _     -> print_char ']'
  in aux liste; 
  print_newline ()
;;

let diviser p sep =
  let i = ref (-1) in
	List.partition (fun x -> incr i; (!i)<=sep) p.coeff
;;

let soustraction_poly p q =
  let rec soustraction l1 l2 = match l1,l2 with
  | t1::q1, t2::q2 -> (t1 -. t2)::(soustraction q1 q2)
  | l, [] -> l
  | [], l -> List.map (fun x -> (-.x)) l
  in
  let diff = soustraction p.coeff q.coeff in
  { deg = max p.deg q.deg; coeff = diff}
;;

let addition_poly p q =
  let rec addition l1 l2 = match l1,l2 with
  | t1::q1, t2::q2 -> (t1 +. t2)::(addition q1 q2)
  | l, [] | [], l -> l
  in
  let somme = addition p.coeff q.coeff in
  { deg = max p.deg q.deg; coeff = somme}
;;

let produit_xk p k = 
  if ( p.deg >= 0 ) then ( { deg = p.deg + k; coeff = List.init k (fun x -> 0.)@p.coeff} )
  else ( p )
;;

let rec produit_poly p q =
  if (p.deg = -1 || q.deg = -1) then (

    { deg = -1; coeff = []};

  ) else if (p.deg = 0) then (

    let a = List.hd p.coeff in
    { deg = q.deg; coeff = List.map (fun x-> x *. a) q.coeff};

  ) else if (q.deg = 0) then (

    let a = List.hd q.coeff in
    { deg = p.deg; coeff = List.map (fun x-> x *. a) p.coeff};

  ) else (

    let k = (min p.deg q.deg)/2 + 1 in

    let degP0 = min (k-1) p.deg
    and degP1 = max (p.deg - k) (-1)
    and degQ0 = min (k-1) q.deg
    and degQ1 = max (q.deg - k) (-1) in

    let coeffP0, coeffP1 = diviser p degP0
    and coeffQ0, coeffQ1 = diviser q degQ0 in

    let p0 = {deg = degP0; coeff = coeffP0}
    and p1 = {deg = degP1; coeff = coeffP1}
    and q0 = {deg = degQ0; coeff = coeffQ0}
    and q1 = {deg = degQ1; coeff = coeffQ1} in
    
    let p0q0 = produit_poly p0 q0
    and p1q1 = produit_poly p1 q1 in

    let prodDelta = produit_poly (soustraction_poly p1 p0) (soustraction_poly q1 q0) in

    let terme_xk = produit_xk (addition_poly p0q0 (soustraction_poly p1q1 prodDelta)) k in
    print_int k; print_newline ();
    print_int terme_xk.deg; print_newline ();

    addition_poly 
      p0q0
      (addition_poly
        (produit_xk (addition_poly p0q0 (soustraction_poly p1q1 prodDelta)) k)
        (produit_xk p1q1 (2*k))
      )

  )
;;

let p = { deg = 2; coeff = [1.; 1.; 1.]};;
let q = { deg = 1; coeff = [0.; 5.] };;

produit_poly p q;;