(* Travail écrit:
Que peut-on immédiatement conclure sur le temps total d’exécution ? En déduire que le problème se
ramène à uniquement respecter les dépendances du graphe.
*)

(*Travail écrit :
Identifiez ces états avec les listes Y , Z et X = V \(Y ∪ Z). Quelle propriété garantit que l’ordre produit
par cet algorithme est topologique, c’est-à-dire qu’il respecte l’ordre partiel défini par les dépendances
du graphe ? Quelle propriété garantit qu’il est total ?

Z symbolise l'état des noeuds numérotés.
Y symbolise l'état des noeuds non numérotés et avec tous ces prédécesseurs numérotés.
X symbolise l'état des noeuds non numérotés et  avec certians de ces prédécésseurs non numérotés.
*)

(* Travail écrit :
En supposant que les fonctions Succ et Prec ont un coût constant, calculer l’ordre de complexité de
l’algorithme en fonction du nombre de nœuds n et du nombre d’arcs m.
*)

(*Travail écrit :
Comment appele-t-on les parcours induits par l’utilisation d’une pile ? Et d’une file ?
*)



(* entrees: 
   - un DAG
   sorties:
   - une liste des sommets du DAG ordonnées selon un tri topologique 
   specifs: 
   - vous implementerez l'algorithme 1 de l'enonce, en utilisant un format de file pour Y (section 1)
   *)
val tri_topologique : DAG -> Vertex list


(* trace d'execution 
   definie en Section 2 de l'enonce (voir Figure 2)
*)
type Trace = (Vertex list) list 

(* entrees: 
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs:
   - le DAG est suppose non pondere 
   - les ressources sont supposees illimitees (section 2.1)
   *)
val ordonnanceur_ressources_illimitees : DAG -> Trace

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
val ordonnanceur_ressources_limitees_sans_heuristique : int -> DAG -> Trace

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
val ordonnanceur_ressources_limitees_avec_heuristique : int -> DAG -> Trace


(* entrees: 
   - un nombre entier de ressources
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs: 
   - le DAG est suppose pondere (section 2.3)
   - les ressources sont supposees limitees 
   *)
val ordonnanceur_graphe_pondere : int -> DAG -> Trace
