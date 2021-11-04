
type 'a array_dyn = {mutable t : 'a array; mutable n : int};;

(* Mon objectif ça va être de refaire les fonctions de base utilisable en Python*)

(* copie t1 dans t2 *)
let copy t1 t2 = 
for i = 0 to (Array.length t2 - 1) do 
  t2.(i) <- t1.(i)
done;;

(* Permet d'ajouter un élément la fin d'un tableau dynamique*)
let append e d =
  if d.n < Array.length d.t then (d.t.(d.n) <- e; d.n <- d.n + 1)
  else if d.n = 0 then (d.t <- [|e|]; d.n <- 1)
  else (let t' = Array.make (2*d.n) d.t.(0) in
        copy d.t t';
        t'.(d.n) <- e;
        d.t <- t';
        d.n <- d.n + 1);;


(*Une fonction qui sert à supprimer un élément à une postion précise d'un tableau dynamique*)
let suppr d n =
let d1 = {t = [| |]; n = 0}
for i = 0 to (Array.length d.t - 1) do 
  if n <> i then append i d1
  else ()
done
d1;;
(* Fonction probablement optimisable mais je ne vois pas comment, il faudrait que je puisse mettre du "vide" dans le tableau et pas juste du unit*)
(* Ci-dessous un exemple d'une tentaive *)

(* 
let pop d =
  if d.n = 0 then (d.t <- [||]; d.n <- ())
  else if d.n < (Array.length d.t)/2 
        (let t' = Array.make ((d.n)/2) d.t.(0) in
        copy d.t t';
        t'.(d.n) <- ();
        d.t <- t';
        d.n <- d.n - 1);;
        else...
*)


(* Pareil que pour les listes mais versions tableaux dynamique*)
let filter d f =
let d1 = {t = [| |]; n = 0}
for i = 0 to (d.n- 1) do 
  if f (d.t.(i)) then append i d1
  else ()
done;
d1;;

(* Inverser les ordres des éléments d'un tableau *)
let reverse d =
    for i = 0 to (d.n)/2 - 1 do
      let a = d.t.(i) (* On pose une variable intermédiaire pour améliorer la complexité *)
      d.t.(i) <- d.t.(d.n - i)
      d.t.(d.n - i) <- a

(* C'est fait *)
let dichotomie d e =
  let aux i j =
    if i > j then false
    else
    let m = (i + j)/2 in
    if d.t.(m) = e then true
    else if d.t.(m) < e then aux (m-1) j
    else if d.t.(m) > e then aux i (m+1)
 in aux 0 (d.n - 1)



(* Pour concaténer 2 tableau non dynamiques *)
let concat t1 t2 =
let t3 = Array.make (Array.length t1 + Array.length t2 - 1) 0 in
copy t1 t3;
for i = 0 to (Array.length t2 - 1) do 
    t3.(i + Array.length t1) <- t1.(i)
done;
