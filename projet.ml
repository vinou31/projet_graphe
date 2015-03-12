(* entrees: 
   - un DAG
   sorties:
   - une liste des sommets du DAG ordonnées selon un tri topologique 
   specifs: 
   - vous implementerez l'algorithme 1 de l'enonce, en utilisant un format de file pour Y (section 1)
   *)
(*
val tri_topologique : DAG -> Vertex list
*)

(*
#use "graphes_test.ml";;
*)

(*
open DAG;;
*)


let rec print_vertex lv = List.fold_right (fun v x -> (V.label v)::x) lv [];;
let rec print_mark_v lv = List.fold_right (fun v x -> (Mark.get v)::x) lv [];;
let rec print_vertex_list lv = List.fold_right (fun v x -> (print_vertex v)::x) lv [];;

let rec separator l1 l2 n liste_fini =
match liste_fini with
|[]->if (List.length l1)>(n) then
		separator (List.tl l1) ((List.hd l1)::l2) (n) []
	else
		if (List.length l2>n) then
			separator l2 [] (n) ([l1]@liste_fini)
		else
			if (l2 = []) then
				[l1]@liste_fini
			else
				([l2]@[l1]@liste_fini)		
|a::b->if (List.length l1)>(n) then		
		separator (List.tl l1) ((List.hd l1)::l2) (n) liste_fini
	else
		if (List.length l2>n) then
			separator l2 [] (n) ([l1]@liste_fini)
		else
			if (l2 = []) then
				[l1]@liste_fini
			else
				([l2]@[l1]@liste_fini)
	;;
	

let rec is_include l1 l2 = 
	match l1 with
		|[]->true
		|t::q ->
			if (List.mem t l2) then
				is_include q l2
			else false
;;

(*sort les noeuds sans dependance*) (*complexité : O (card (V))*)
let v_sansDep dag =
	let list_v = fold_vertex (fun v x -> v::x) dag [] in		
		List.fold_right (fun v x ->
				if (in_degree dag v = 0) then
					v::x
				else
					x) list_v []
		
;;





(*
let tri_topologique dag =
	let y = v_sansDep dag in
				let rec constructeur_z vi i z = 			
					match vi with
					|[]-> z(*retourner z*)
					|h::t->	begin 
							(*numéroter h, ajouter h à z*)	
							Mark.set h i;						
							(*manque une fonction recursive pour parcourir tout vj*)
							let vj = succ dag h in
								let rec verification lvj lvi lz=  
								match lvj with
								|[]-> constructeur_z (t@lvi) (i+1) (lz)
								|a::b-> if is_include (pred dag a) lz then
												(*aux a::t*)
												verification b (lvi@[a]) lz											
											else
												verification b lvi lz
							in verification vj [] (h::z);
						end						
				in constructeur_z y 1 []
;;				
*)
(* Essaie de codage avec un fold*)
let tri_topologique dag =
	let y = v_sansDep dag in
				let rec constructeur_z vi i z = 			
					match vi with
					|[]-> z(*retourner z*)
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
*)
let rec find_next_step_illimite dag etapePrecedente trace nextStep = 
		match etapePrecedente with 
			|[] -> nextStep
			|a::la -> (*s'il reste des elements de l'etape precedente*)
				let lSucc = succ dag a in (*sur la liste des successeurs de chacun de ces elements*)
					let new_nxt = List.fold_right (fun v x ->if (is_include (pred dag v) (etapePrecedente@(List.flatten trace))) then
																		(x@[v])										
																	else
																		x)
																  lSucc nextStep 
								in find_next_step_illimite dag la trace new_nxt									
;;

let ordonnanceur_ressources_illimitees dag =
	let rec ordonnanceur trace =
		let y = trouve_sans_dep dag trace in
					if (y =[]) then
						trace
					else
						ordonnanceur ([y]@trace)
		in ordonnanceur []
;;				


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
*)
let trouve_sans_dep dag trace =
	let list_v = fold_vertex (fun v x -> v::x) dag [] in		
		List.fold_right (fun v x ->
				if ((is_include (pred dag v) (List.flatten trace)) && ((List.mem v (List.flatten trace))=false)) then
					v::x
				else
					x) list_v []
;;

(*
let rec update_sansDep dag ressources etapeSuivanteTrouvee reste trace_flattened = 
	match  reste with 
	|[] -> 
	|a::b ->
	*)
		

let rec find_next_step_limite_sans_heuristique  dag etapePrecedente trace nextStep = 
		match etapePrecedente with 
			|[] -> nextStep
			|a::la -> (*s'il reste des elements de l'etape precedente*)
				let lSucc = succ dag a in (*sur la liste des successeurs de chacun de ces elements*)
					let new_nxt = List.fold_right (fun v x ->if (is_include (pred dag v) (etapePrecedente@(List.flatten trace)))  then
																		(x@[v])										
																	else
																		x)
																  lSucc nextStep 
								in find_next_step_limite_sans_heuristique dag la trace new_nxt									
;;

let ordonnanceur_ressources_limitees_sans_heuristique ressource dag =
	let rec ordonnanceur trace =
		match trace with
		|[] -> let y = trouve_sans_dep dag trace in
					if (y =[]) then
						trace
					else
						ordonnanceur ((separator (y) [] ressource [])@trace)		
		|a::b ->if (List.length a) < ressource then 
					let y = (trouve_sans_dep dag trace) in
						if (y =[]) then
							trace
						else
							let ajust = separator y a ressource []  in
							ordonnanceur (ajust@trace)
				else
					let y = (trouve_sans_dep dag trace) in
					if (y =[]) then
						trace
					else							
						ordonnanceur ((separator y [] ressource [])@trace)	
		in ordonnanceur []
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
