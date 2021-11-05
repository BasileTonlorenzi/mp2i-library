type ('a, 'b) table_hachage = {hache: 'a -> int; donnees: ('a * 'b) list array; largeur: int}

(* Pour créer la table de hachage *)
let creer h w =
{hache = h; 
donnees = Array.make w [];
largeur = w};;

(* Pour chercher la présence d'une clé *)
let recherche t k =
    let liste = t.donnees.(t.hache k) in
    let liste1 = List.map fst liste in
    List.mem k liste1;;

(* Pour chercher un l'élément associé à la clé *)
let element t k =
    let rec aux = function
    | [] -> failwith  "Pas de k"
    | (k', a)::q -> if k = k' then a else aux q
in aux t.donnees.(t.hache k);;

(* Pour ajouter aux données l'élément e avec une clé si besoin*)
let ajout t k e = 
if recherche t k = true then failwith "Pas de k"
else t.donnees.(t.hache k) <- (k, e)::(t.donnees.(t.hache k));;

(* Pour supprimee l'entrée de k *)
let suppr t k =
if not (recherche t k) then failwith "Pas de changement"
else let rec enlever = function
        | [] -> []
        | (k', _)::q when k=k' -> enlever q
        | e::q -> e::enlever q
in t.donnees.(t.hache k) <- enlever (t.donnees.(t.hache k));;

(* Une fonction qui renvoie une table de hachage contenant tout les éléments de la première qui remplisse une certaine condition *)
Tableau [List (couple)]

let filter t f =
for i=0 to Array.length t.donnees do
  let rec epreuve l = function
      | [] -> []
      | (_, e2)::q when f e2 -> e2::(epreuve q)
      | e::q -> epreuve q

in {hache = t.hache
donnees = t.
}
