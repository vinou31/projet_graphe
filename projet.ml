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
	

let rec is_include l1 l2 = 
	match l1 with
		|[]->true
		|t::q ->
			if (List.memq t l2) then
				is_include q l2
			else false
;;

let rec is_not_include l1 l2 = 
	match l1 with
		|[]->true
		|t::q ->if  (List.memq t l2) then
				false
			else 
				is_not_include q l2
;;


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
let trouve_sans_dep dag trace =
	let list_v = fold_vertex (fun v x -> v::x) dag [] in		
		List.fold_right (fun v x ->
				if ((is_include (pred dag v) (List.flatten trace)) && (not (List.mem v (List.flatten trace)))) then
					v::x
				else
					x) list_v []
;;

(*
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
*)



let ordonnanceur_ressources_illimitees dag =
		let rec ordonnanceur trace=
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
let ordonnanceur_ressources_limitees_sans_heuristique ressource dag =
	let rec ordonnanceur trace c =
		match trace with
		|[] -> let y = trouve_sans_dep dag trace in
					if (y =[]) then
						(trace,c)
					else
						ordonnanceur ((separator (y) [] ressource [])@trace) (c+1)	
		|a::b ->let y = (trouve_sans_dep dag trace) in
						if (y =[]) then
							(trace,c)
						else
							let (indep,new_y) = is_depend y a dag in							
								if ((List.length a) < ressource) && (indep<>[])  then									 
									let ajust = List.rev (separator indep a ressource [])  in
										(ordonnanceur ((separator (new_y@(List.flatten(List.rev (List.tl ajust)))) [] ressource [])@[List.hd ajust]@b)) (c+1)
								else						
									ordonnanceur ((separator y [] ressource [])@trace)	(c+1)
		in ordonnanceur [] 1
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

let prof_v dag v = 
let l = succ dag v in
let rec prof lv i = 
	match lv with
		|[] ->i
		|a::b -> (max (prof(succ dag a) (i+1)) (prof b i))
		in prof l 0
;;




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


let ordonnanceur_ressources_limitees_avec_heuristique ressource dag =
	
	let rec ordonnanceur trace c =
		match trace with
		|[] -> let y = tri_sans_dep dag (trouve_sans_dep dag trace) in
					if (y =[]) then
						(trace,c)
					else
						ordonnanceur ((separator (y) [] ressource [])@trace) (c+1)		
		|a::b ->let y = tri_sans_dep dag (trouve_sans_dep dag trace) in
						if (y =[]) then
							(trace,c)
						else
							let (indep,new_y) = is_depend y a dag in							
								if ((List.length a) < ressource) && (indep<>[])  then									 
									let ajust = List.rev (separator indep a ressource [])  in
										(ordonnanceur ((separator (new_y@(List.flatten(List.rev (List.tl ajust)))) [] ressource [])@[List.hd ajust]@b)) (c+1)
								else						
									ordonnanceur ((separator y [] ressource [])@trace)	(c+1)
		in ordonnanceur [] 1
		
;;	


let time f ressource dag =
    let t = Sys.time() in
    let fx = f ressource dag in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx

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
