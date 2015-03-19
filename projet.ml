(* Travail écrit:
Que peut-on immédiatement conclure sur le temps total d’exécution ? En déduire que le problème se
ramène à uniquement respecter les dépendances du graphe.

Au minimum la somme des temps d'execution de chaque tache
*)

(*Travail écrit :
Identifiez ces états avec les listes Y , Z et X = V \(Y ∪ Z). Quelle propriété garantit que l’ordre produit
par cet algorithme est topologique, c’est-à-dire qu’il respecte l’ordre partiel défini par les dépendances
du graphe ? Quelle propriété garantit qu’il est total ?

Y : liste des vertex avant l'etablissement de l'ordre topologique => 
VERTICES NON NUMEROTES ET PREDECESSEURS NON NUMEROTES
Z : le tri topologique est fini =>
TOUS LES NOEUDS SONT NUMEROTES
X : NON NUMEROTES ET AVEC TOUS LES PREDECESSEURS NUMEROTES
Z contenant les noeuds déja numerotes, on verifie dans cet algo que tous les prédécésseurs d'un noeud soient déja numerotes,
de ce fait on vérifie d'abord que tout l'etage est numeroté avant de passer au(x) noeud(s) induit(s) : 
Ordre partiel
Un tri topologique permet de transformer un DAG en un chemin constitué de tous les noeuds numerotes:
Ordre total
*)

(* Travail écrit :
En supposant que les fonctions Succ et Prec ont un coût constant, calculer l’ordre de complexité de
l’algorithme en fonction du nombre de nœuds n et du nombre d’arcs m.

n*m²
*)

(*Travail écrit :
Comment appele-t-on les parcours induits par l’utilisation d’une pile ? Et d’une file ?

Parcours en profondeur pour la pile
Parcours en largeur pour la file
*)




(*imprime les vertex d'une liste*)
let print_vertex lv = List.fold_right (fun v x -> (V.label v)::x) lv [];;

(*
imprime les mark des vertex d'une liste
*)
let print_mark_v lv = List.fold_right (fun v x -> (Mark.get v)::x) lv [];;

(*
imprime les vertex d'une liste de liste (utilisé pour la trace)
*)
let rec print_vertex_list lv = List.fold_right (fun v x -> (print_vertex v)::x) lv [];;
(*
fait la somme des pondérations des vertex
*)
let somme dag = fold_vertex (fun v x->x+(snd (V.label v))) dag 0;;  

(*
separe la liste l1 en listes de n elements chacune. 
l2 sert de buffer.
Quand l2 atteint la taille n, il est stocké dans liste_fini
val 'a list -> 'a list -> int -> 'a list list -> 'a list list
*)
let rec separator l1 l2 n liste_fini =
match liste_fini with
|[]->if (List.length l2)<(n) then
		if (l1<>[]) then
			separator (List.tl l1) ((List.hd l1)::l2) (n) []
		else
			[l2]@liste_fini
	else
		if (List.length l1>n) then
			separator l1 [] (n) ([l2]@liste_fini)
		else
			if (l1 = []) then
				[l2]@liste_fini
			else
				([l1]@[l2]@liste_fini)		
|a::b->if (List.length l2)<(n) then
			if (l1<>[]) then
				separator (List.tl l1) ((List.hd l1)::l2) (n) liste_fini
			else
				[l2]@liste_fini
		else
			if (List.length l1>n) then
				separator l1 [] (n) ([l2]@liste_fini)
			else
				if (l1 = []) then
					[l2]@liste_fini
				else
					([l1]@[l2]@liste_fini)
	;;
	
(*
verifie si tous les elements de l1 sont contenus dans l2 en utilisant
 les egalites fournies par le module List 
*)
let rec is_include l1 l2 = 
	match l1 with
		|[]->true
		|t::q ->
			if (List.mem t l2) then
				is_include q l2
			else false
;;

(*
verifie si tous les elements de l1 sont contenus dans l2 en utilisant
 les egalites sur les label
*)

let rec is_include_V2 l1 l2 = 
	match l1 with
		|[]->true
		|t::q -> 
			let rec aux l =
				match l with
				|[]->false
				|a::b-> if (V.label a) = (V.label t) then
							is_include_V2 q l2
						else 
							aux b
			in aux l2				
;;

(*
renvoie true si aucun element de l1 n'appartient à l2
*)
let rec is_not_include l1 l2 = 
	match l1 with
		|[]->true
		|t::q ->if  (List.mem t l2) then
				false
			else 
				is_not_include q l2
;;

(*
renvoie la liste des elements de l1 qui n'ont aucune dépendance dans l2
val 'a list -> 'a list -> dag -> 'a list
*)
let is_depend l1 l2 dag =
	let l = List.fold_right (fun v x ->
				if ((is_not_include (pred dag v) (l2))) then
					(v::x)
				else
					x) l1 [] in
	let rec aux lv s = 
		match lv with
		|[]-> l,s
		|a::b-> if (List.mem a l) then
					aux b s
				else
					aux b (a::s)
	in aux l1 []	
;;

(*sort les noeuds sans dependance à partir de la trace déjà réalisée*) 
let trouve_sans_dep dag trace =
	let list_v = fold_vertex (fun v x -> v::x) dag [] in		
		List.fold_right (fun v x ->
				if ((is_include (pred dag v) (List.flatten trace)) && (not (List.mem v (List.flatten trace)))) then
					v::x
				else
					x) list_v []
;;

(* entrees: 
   - un DAG
   sorties:
   - une liste des sommets du DAG ordonnées selon un tri topologique 
   specifs: 
   - vous implementerez l'algorithme 1 de l'enonce, en utilisant un format de file pour Y (section 1)
   *)
(*
val tri_topologique : DAG -> Vertex list

Tests : 
dag1 : [("s", 1); ("q", 1); ("n", 1); ("j", 1); ("b", 1); ("k", 1); ("p", 1);
 ("g", 1); ("i", 1); ("h", 1); ("d", 1); ("f", 1); ("c", 1); ("e", 1);
 ("l", 1); ("m", 1); ("o", 1); ("r", 1)]

 
 dag3 : [("k", 1); ("n", 1); ("j", 1); ("g", 1); ("d", 1); ("m", 1); ("f", 1);
 ("a", 1); ("l", 1); ("c", 1); ("v", 1); ("e", 1); ("i", 1); ("h", 1)]
 
 
*)


let tri_topologique dag =
	let y = trouve_sans_dep dag [] in
				let rec constructeur_z vi i z = 			
					match vi with
					|[]-> List.rev z(*retourner z*)
					|h::t->	begin 
							(*numéroter h, ajouter h à z*)	
							Mark.set h i;
							let new_z = h::z in	
								let successeur = succ dag h in
									let new_vi = List.fold_right (fun v x ->if is_include (pred dag v) new_z then
																		x@[v]										
																	else
																		x)
																  successeur [] in
									constructeur_z (t@new_vi) (i+1) new_z;
							end
				in constructeur_z y 1 []
;;	


(* trace d'execution 
   definie en Section 2 de l'enonce (voir Figure 2)
*)
(*
type Trace = (Vertex list) list 
*)

(*
Travail écrit
Quels nœuds sont traités à la première étape ? A la k-ième étape ? Conclure sur la valeur du temps
d’exécution total (i.e., le nombre total d’étapes).

A la première étape, on traite la liste des noeuds qui n'ont aucune dépendance en entrée. A la k-ième étape
on traite les noeuds dont une dépendance reste non traité.
Le temps d'execution sera donc égale à la profondeur max du DAG
*)

(* entrees: 
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs:
   - le DAG est suppose non pondere 
   - les ressources sont supposees illimitees (section 2.1)
   *)
(*
val ordonnanceur_ressources_illimitees : DAG -> Trace


Tests:
dag1 : [[("s", 1); ("q", 1); ("n", 1); ("j", 1); ("b", 1)]; [("p", 1); ("k", 1)];
 [("i", 1); ("g", 1)]; [("h", 1); ("f", 1); ("d", 1)];
 [("o", 1); ("m", 1); ("l", 1); ("e", 1); ("c", 1)]; [("r", 1)]]
 
dag2 : [[("a", 1)]; [("n", 1); ("j", 1)]; [("o", 1)]; [("r", 1)];
 [("f", 1); ("e", 1)]; [("h", 1); ("x", 1)]; [("i", 1)];
 [("c", 1); ("b", 1)]]

*)
let ordonnanceur_ressources_illimitees dag =
		let rec ordonnanceur trace=
			let y = trouve_sans_dep dag trace in
						if (y =[]) then
							List.rev trace
						else
							ordonnanceur ([y]@trace)
			in ordonnanceur []
;;				

(*
Travail écrit
Donner une borne inférieure du temps total d’exécution en fonction du nombre de nœuds n et du nombre
de ressources r. Dans le cas où cette borne inférieure est atteinte, que peut-on dire sur l’utilisation des
ressources à chaque étape ?

borne inférieure : n/r car le cas optimal serait le cas où à chaque étape on utiliserait toutes les ressources disponibles.
*)
(*
Comment se traduit la contrainte de ressources limitées sur les listes à chaque étape ?

on impose une taille maximal à la liste de la trace.
*)

(* entrees: 
   - un nombre entier de ressources
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs: 
   - le DAG est suppose non pondere
   - les ressources sont supposees limitees (section 2.2)
   - vous n'utiliserez pas d'heuristique
   *)
(*
val ordonnanceur_ressources_limitees_sans_heuristique : int -> DAG -> Trace

Tests : 
dag1 2 : [[("q", 1); ("s", 1)]; [("j", 1); ("n", 1)]; [("p", 1); ("b", 1)];
 [("i", 1); ("k", 1)]; [("f", 1); ("g", 1)]; [("d", 1); ("h", 1)];
 [("m", 1); ("o", 1)]; [("e", 1); ("l", 1)]; [("r", 1); ("c", 1)]]
 
dag1 5 : [[("b", 1); ("j", 1); ("n", 1); ("q", 1); ("s", 1)]; [("k", 1); ("p", 1)];
 [("g", 1); ("i", 1)]; [("d", 1); ("f", 1); ("h", 1)];
 [("c", 1); ("e", 1); ("l", 1); ("m", 1); ("o", 1)]; [("r", 1)]]
 
*)
let ordonnanceur_ressources_limitees_sans_heuristique ressource dag =
	let rec ordonnanceur trace =
		match trace with
		|[] -> let y = trouve_sans_dep dag trace in
					if (y =[]) then
						List.rev trace
					else
						ordonnanceur ((separator (y) [] ressource [])@trace) 	
		|a::b ->let y = (trouve_sans_dep dag trace) in
						if (y =[]) then
							List.rev trace
						else
							let (indep,new_y) = is_depend y a dag in							
								if ((List.length a) < ressource) && (indep<>[])  then									 
									let ajust = List.rev (separator indep a ressource [])  in
										if new_y = [] then
											ordonnanceur  ((List.rev ajust)@b) 
										else										
											ordonnanceur ((separator (new_y@(List.flatten(List.rev (List.tl ajust)))) [] ressource [])@[List.hd ajust]@b) 
								else						
									ordonnanceur ((separator y [] ressource [])@trace)	
		in ordonnanceur []
;;	



(*
Travail écrit
Proposer une heuristique pour choisir les tâches traitées en priorité (i.e., les r premières tâches de Y ).
on choisit en prioprité de traiter les noeuds ayant la profondeur la plus grande.
*)

(*
Analyser l’efficacité de l’heuristique sur différents graphes, en faisant varier le nombre de ressources r.
*)



(*
calcul la profondeur maximal d'un vertex dans un dag
*)
let prof_v dag v = 
let l = succ dag v in
let rec prof lv i = 
	match lv with
		|[] ->i
		|a::b -> (max (prof(succ dag a) (i+1)) (prof b i))
		in prof l 0
;;



(*
tri les éléments de y dans lordre décroissant de leur profondeur
*)
let tri_sans_dep dag l =
	 List.sort (fun a b -> 
		if (prof_v dag a)<(prof_v dag b) then
			1
			else
				if (prof_v dag a)>(prof_v dag b) then
					-1
					else 
						0) l
;;

(* entrees: 
   - un nombre entier de ressources
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs: 
   - le DAG est suppose non pondere
   - les ressources sont supposees limitees (section 2.2)
   - vous utiliserez une heuristique pour ameliorer la duree de la trace 
   *)
(*
val ordonnanceur_ressources_limitees_avec_heuristique : int -> DAG -> Trace
*)
let ordonnanceur_ressources_limitees_avec_heuristique ressource dag =	
	let rec ordonnanceur trace =
		match trace with
		|[] -> let y = tri_sans_dep dag (trouve_sans_dep dag trace) in
					if (y =[]) then
						List.rev trace
					else
						ordonnanceur ((separator (y) [] ressource [])@trace) 		
		|a::b ->let y = tri_sans_dep dag (trouve_sans_dep dag trace) in
						if (y =[]) then
							List.rev trace
						else
							let (indep,new_y) = is_depend y a dag in							
								if ((List.length a) < ressource) && (indep<>[])  then									 
									let ajust = List.rev (separator indep a ressource [])  in
									if new_y = [] then
										ordonnanceur  ((List.rev ajust)@b) 
									else	
										ordonnanceur ((separator (new_y@(List.flatten(List.rev (List.tl ajust)))) [] ressource [])@[List.hd ajust]@b) 
								else						
									ordonnanceur ((separator y [] ressource [])@trace)	
		in ordonnanceur [] 
		
;;	


(*
Travail ecrit
Comment représenter un nœud pondéré par un graphe aux nœuds non pondérés ? En déduire une
méthode simple pour résoudre le problème sur des graphes aux nœuds pondérés.

on multiplie ce noeud autant de fois que sa pondération le demande avec les mêmes dépendances pour chaque copie.
méthode simple : on transforme en graphe non pondéré et on applique les derniers algorithmes avec le graphe obtenu.
*)

(*
Que remarquez-vous ? Expliquer les disparités au niveau du temps de calcul de votre programme entre
les différents graphes tests fournis.

lorsque la pondération est très grande, le temps de calcul est aussi augmenté grandement.
On peut expliquer cela car on n'utilise pas la propriété que nous avons beaucoup de fois les mêmes noeuds qui pourrait être vite traité.
*)

(*
creer autant de vertex que le montant de sa pondération et les relie aux autres éléments du dag
val DAG->V.t->unit
*)
let creer_n_v dag v =
	 let n = snd (V.label v) in
			let rec creer i =
				match i with
				|0->()
				|_->let vi =  V.create((fst(V.label v))^(string_of_int i),1) in 
						begin
						add_vertex dag vi;
						List.iter (fun v -> add_edge dag vi v) (succ dag v);
						List.iter (fun v -> add_edge dag v vi) (pred dag v);
						creer (i-1);
						end
				in creer n
;;
				
(*
transforme un dag pondéré en non pondéré en multipliant chaque vertex par leur pondération
DAG->DAG
*)
let transfo_non_pondere dag =
	let dag_non_pond = copy dag in
	begin
		iter_vertex (fun v -> 
						begin						
						(creer_n_v dag_non_pond v);						
						remove_vertex dag_non_pond v;
						end) dag_non_pond ;
		dag_non_pond
	end
;;

(*
marque tous les vertex d'un dag avec leur pondération
*)
let marqueur dag =
	iter_vertex (fun v -> Mark.set v (snd (V.label v))) dag
;;

(*
copie un vertex dans une liste de la taille de sa pondération 
contenant les clone de v de pondération 1
val 'a -> 'a list -> 'a list
*)
let rec copy_n_v v l=
	if (Mark.get v) = 0 then
		l
	else
		let v1 = V.create ((fst (V.label v)),1) in
			begin
			Mark.set v ((Mark.get v)-1);
			copy_n_v v (v1::l);
			end
;;
(*
créer une list dont tous les éléments sont ceux de l avec une pondération de 1
*)
let clone_l l =
	List.map (fun x-> V.create ((fst (V.label x)),1)) l
;;

(*
créer un clone de v avec une pondération de 1
*)
let clone_v_1 v =
	V.create ((fst (V.label v)),1)
;;

(*
mise à jour de trouve_sans_dep adapter aux éléments ayant une pondération >= 1
*)
let trouve_sans_dep_pondere dag trace =
	let list_v = fold_vertex (fun v x -> v::x) dag [] in		
		List.fold_right (fun v x ->
				if ((is_include_V2 (clone_l (pred dag v)) (List.flatten trace)) && (not (is_include_V2 [clone_v_1 v] (List.flatten trace)))) then
					v::x
				else
					x) list_v []
;;


(*
tri y en listes de taille ressource et les concatène à la trace
val 'a list -> DAG -> 'a list list ->'a list list
*)
let rec tri_y_pondere ressource y dag trace= 
		match y with
		|[]-> trace
		|h::q-> match trace with
				|[]-> if (Mark.get h) >1 then
						let l = (copy_n_v h []) in
							tri_y_pondere ressource q dag (separator l [] ressource trace)
						else
							tri_y_pondere ressource q dag ([h]::trace)
				|a::b-> if (Mark.get h) >1 then
						let l = (copy_n_v h []) in
							tri_y_pondere ressource q dag (separator l a ressource b)
						else
							tri_y_pondere ressource q dag (separator [h] a ressource b)
;;

(* entrees: 
   - un nombre entier de ressources
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs: 
   - le DAG est suppose pondere (section 2.3)
   - les ressources sont supposees limitees 
   *)
(*
val ordonnanceur_graphe_pondere : int -> DAG -> Trace
*)
let ordonnanceur_graphe_pondere ressource dag =
	begin
	Mark.clear dag;
	marqueur dag;
	let rec ordonnanceur trace =
		let y = trouve_sans_dep_pondere dag trace in
			if y = [] then
				List.rev trace
			else
				 ordonnanceur (tri_y_pondere ressource y dag trace)
	in ordonnanceur [];
	end
;;	
				
