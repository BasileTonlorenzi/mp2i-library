(* Juste une fonction servant à concaténer 2 listes *)
let rec concat l1 l2 = match l1,l2 with
| [],  [] -> []
| [], l2 -> l2
| l1, [] -> l1
| e::q, e1::q1 -> e::e1::(concat q q1) ;;

(* tri fusion*)

(* On casse la liste en 2*)
let rec split l = match l with
| [] -> [],[]
| [e] -> l,[]
| e1::e2::c-> let (l1,l2) = 
            split c in e1::l1,e2::l2;;

(* On fusionne 2 listes pour en créer une nouvelle triée *)
let rec fusion l1 l2 = match l1,l2 with
| [], [] -> []
| [], l2 -> l2
| l1, [] -> l1
| e1::q1, e2::q2 -> if e1 > e2 then e1::e2::(fusion q1 q2)
                               else e2::e1::(fusion q1 q2);;

(* On casse jusqu'à avoir des listes de taille 1 puis on reconstruit en triant à chaque étape*)
let rec tri_fusion l = match l with
| [] -> []
| [e] -> [e]
| _ -> let (l1,l2) = split l in
            fusion (tri_fusion(l1)) (tri_fusion(l2));;


(* Je vais tenter le tri rapide aussi étant donné que je l'ai pas fait lors du controle*)

(* Vu que je la réutilise je la redéfinie*)
let rec filter f l = match l with
 | [] -> []
 | e::q -> if f e then e::filter f q else filter f q;;

(* On sépare la liste en 2 listes avec les éléments plus petits ou plus grands que quelque chose *)
let partition l p = 
(List.filter (fun x -> x < p) l, List.filter (fun x -> x >= p) l);;

(* On sépare jusqu'à avoir des listes de taille  puis on *)
let rec tri_rapide l = match l with
| [] -> []
| e::q -> let (l1,l2) = partition q e in
            (tri_rapide l1)@(e::(tri_rapide l2));;
