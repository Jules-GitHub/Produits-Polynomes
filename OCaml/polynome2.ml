let diviser p sep =
  let i = ref (-1) in
	List.partition (fun x -> incr i; (!i)<=sep) p
;;

let rec soustraction_poly p q = match p,q with
| t1::q1, t2::q2 -> (t1 -. t2)::(soustraction_poly q1 q2)
| l, [] -> l
| [], l -> List.map (fun x -> (-.x)) l
;;

let rec addition_poly p q = match p,q with
| t1::q1, t2::q2 -> (t1 +. t2)::(addition_poly q1 q2)
| l, [] | [], l -> l
;;

let produit_xk p k = List.init k (fun x -> 0.)@p;;

let rec produit_poly p q = match (List.length p)-1, (List.length q)-1 with
| -1, _ | _, -1 -> []
| 0, _ -> List.map (fun x-> x *. List.hd p) q
| _, 0 -> List.map (fun x-> x *. List.hd q) p
| d1, d2 -> (

  let k = (min d1 d2)/2 + 1 in

  let p0, p1 = diviser p (min (k-1) d1)
  and q0, q1 = diviser q (min (k-1) d2) in
  
  let p0q0 = produit_poly p0 q0
  and p1q1 = produit_poly p1 q1 in

  let prodDelta = produit_poly (soustraction_poly p1 p0) (soustraction_poly q1 q0) in
  
  addition_poly
    p0q0
    (addition_poly
      (produit_xk (addition_poly p0q0 (soustraction_poly p1q1 prodDelta)) k)
      (produit_xk p1q1 (2*k))
    )

)
;;

let p = [1.; 1.; 1.];;
let q = [0.; 5.];;

produit_poly p q;;