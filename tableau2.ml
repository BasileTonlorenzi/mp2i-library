type 'a array_dyn = {mutable t : 'a array; mutable n : int};;
(* Je suis obligé de séparer en 2 parties sinon ça bug pour les tests *)

(* Inverser les ordres des éléments d'un tableau *)
let reverse d =
    for i = 0 to (d.n)/2 do
      let a = d.t.(i) in (* On pose une variable intermédiare pour échanger 2 à 2 les éléments du tableau *)
      d.t.(i) <- d.t.(d.n - i);
      d.t.(d.n - i) <- a;
    done;
d;;

(* Recherche d'élément par dichotomie *)
let dichotomie d e =
  let rec aux i j =
    if i > j then false
    else
    let m = (i + j)/2 in
    if d.t.(m) = e then true
    else if d.t.(m) < e then aux (m-1) j
    else aux i (m+1)
 in aux 0 (d.n - 1);;
 
 (* Fonction coupant un tableau dynamique en 2 à la position m*)
let split d m =
(*On définie d'abord les tableaux dynamiques à modifier*)
let d1 = {t = Array.make m d.t.(0); n = m-1} in
let difference = Array.length d.t - Array.length d1.t in
let d2 = {t = Array.make difference d.t.(0); n = d.n - (m-1)} in

for i = 0 to Array.length d.t do
    if i <= (m-1) then d1.t.(i) <- d.t.(i)
    else d2.t.(i) <- d.t.(i)
done;
(d1, d2);; (*d1 a tout les élément jusque m, m compris, d2 a tout les autres éléments*)
