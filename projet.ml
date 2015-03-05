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



let rec print_mark_v lv = List.fold_right (fun v x -> (Mark.get v)::x) lv [];;

let tri_topologique dag =
	let y = v_sansDep dag in
				let rec aux vi i z = 			
					match vi with
					|[]-> z(*retourner z*)
					|h::t-> 				
						begin (*numéroter h, ajouter h à z*)	
							(*Mark.set h i;*)							
							(*manque une fonction recursive pour parcourir tout vj*)
							let vj = succ dag h in
								let rec aux2 lvj lvi lz=  
								match lvj with
								|[]-> aux (t@lvi) (i+1) (lz)
								|a::b-> if is_include (pred dag a) lz then
												(*aux a::t*)
												aux2 b (lvi@[a]) lz											
											else
												aux2 b lvi lz
							in aux2 vj [] (h::z);
						end						
				in aux y 1 []
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

(*
let rec find_next_step dag  etapePrecedente nextStep = 
		match etapePrecedente with 
			|[] -> nextStep
			|a::la -> (*s'il reste des elements de l'etape precedente*)
				let lSucc = (succ a) in (*sur la liste des successeurs de chacun de ces elements*)
				let rec analyse lv = (*on re*)
					match lv with
						|[]->(find_next_step dag  la nextStep)
						|a::b -> 
							if(is_include (prec a) etapePrecedente) then
								begin
									a::nextStep;
									analyse b;
								end
							else
								analyse b
				in (analyse lSucc)::(find_next_step dag la nextStep)
;;

let rec build_trace dag trace = 
	let trace = [v_sansDep dag] in
		match trace with
			|[] -> build_trace dag ([v_sansDep dag]::trace)
			|[a]-> let next = find_next_step dag a [] in
				if ((next)=[]) then
					trace
				else
					build_trace dag ((next)::trace)
			|etape::autres_etapes -> let next = find_next_step dag a [] in
				if ((next)=[]) then
					trace
				else
					build_trace dag ((next)::trace)
;;				
*)

(*
let ordonnanceur_ressources_illimitees dag =
	let y = fold_vertex (fun v x -> v::x) dag [] in
		let trace = [] in
			let i = 1 in
				let rec aux vi = 
					let etage = [] in		
					match vi with
					|[]-> (*retourner z*)
					|h::t-> 
						begin (*numéroter h, ajouter h à z*)	
							h::etage;
							let vj = succ h in
								match vj with
								|[]-> aux t
								|a::b-> if List.mem (prec a) z then
												(*aux a::t*)
												aux t@[a]
											else
												aux t
						end
				in aux y
;;				
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
*)

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
