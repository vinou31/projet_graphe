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



(*
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
val ordonnanceur_ressources_illimitees : DAG -> Trace



(*
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
val ordonnanceur_ressources_limitees_sans_heuristique : int -> DAG -> Trace



(*
Proposer une heuristique pour choisir les tâches traitées en priorité (i.e., les r premières tâches de Y ).
on choisit en prioprité de traiter les noeuds ayant la profondeur la plus grande.
*)

(*
Analyser l’efficacité de l’heuristique sur différents graphes, en faisant varier le nombre de ressources r.
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
val ordonnanceur_ressources_limitees_avec_heuristique : int -> DAG -> Trace


(*
Comment représenter un nœud pondéré par un graphe aux nœuds non pondérés ? En déduire une
méthode simple pour résoudre le problème sur des graphes aux nœuds pondérés.

on multiplie ce noeud autant de fois que sa pondération le demande avec les mêmes dépendances pour chaque copie.
méthode simple : on transforme en graphe non pondéré et on applique les derniers algorithmes avec le graphe obtenu.
*)

(*
Que remarquez-vous ? Expliquer les disparités au niveau du temps de calcul de votre programme entre
les différents graphes tests fournis.

losrque la pondération est très grande, le temps de calcul est aussi augmenté grandement.
On peut expliquer cela car on n'utilise pas la propriété que nous avons beaucoup de fois les mêmes noeuds qui pourrait être vite traité.
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
val ordonnanceur_graphe_pondere : int -> DAG -> Trace
