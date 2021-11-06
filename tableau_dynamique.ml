
type 'a array_dyn = {mutable t : 'a array; mutable n : int};;

(* Mon objectif ça va être de refaire les fonctions de base utilisable en Python*)

(* remplie t2 avec autant d'élement de t1 que possible. *)
let copy t1 t2 =
if Array.length t1 < Array.length t2 then 
    (for i = 0 to (Array.length t1 - 1) do
        t2.(i) <- t1.(i)
    done;)
else
(for i = 0 to (Array.length t2 - 1) do 
  t2.(i) <- t1.(i)
done;)
;;

(* Permet d'ajouter un élément la fin d'un tableau dynamique*)
let append e d =
  if d.n < Array.length d.t then (d.t.(d.n) <- e; d.n <- d.n + 1)
  else if d.n = 0 then (d.t <- [|e|]; d.n <- 1)
  else (let t' = Array.make (2*d.n) d.t.(0) in
        copy d.t t';
        t'.(d.n) <- e;
        d.t <- t';
        d.n <- d.n + 1);;

(* Fonction servant à supprimer le dernier élément du tableau*)
let pop d =
  if d.n = 0 then (d.t <- [||]; d.n <- 0)
  else if d.n < (Array.length d.t)/2 then 
        (let t' = Array.make (d.n) d.t.(0) in (* En créeant un nouveau tableau 2 fois plus petit on optimise la complexité mémoire *)
        copy d.t t';
        d.t <- t';
        d.n <- d.n - 1)
  else d.n <- (d.n - 1);;

(* Pour concaténer 2 tableaux dynamiques *)
let concat d1 d2 =
let d3 = {t = Array.make (Array.length d1.t + Array.length d2.t - 1) d1.t.(0);
n = d1.n + d2.n} in
copy d1.t d3.t;
for i = 0 to (d2.n) do 
    d3.t.(i + d1.n) <- d2.t.(i)
done;
d3;;

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

(* Fonction permettant de supprimer un élément à une position précise d'un tableau *)
let suppr d m =
let d1 = {t = [| |]; n = 0} in
for i = 0 to (Array.length d.t - 1) do 
  if m <> i then append i d1
  else ()
done;
d1;;

(* Pareil que pour les listes mais versions tableaux dynamique*)
let filter d f =
let d1 = {t = [| |]; n = 0} in
for i = 0 to (d.n - 1) do 
  if f (d.t.(i)) then append i d1
  else ()
done;
d1;;

(* Inverser les ordres des éléments d'un tableau *)
let reverse d =
    for i = 0 to (d.n)/2 - 1 do
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
 in aux 0 (d.n - 1)

