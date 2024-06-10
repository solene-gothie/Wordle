(*Restriction : à exécuter en top_level*)

(*Ce fichier contient une 3ème méthode de résolution du Wordle, inspirée d'un travail de Michael Bonthro. Elle utilise une approximation de rang 1 pour choisir le prochain coup à jouer*)
(*Les résultats sur un grand nombre de parties n'ont pas été concluants : entre 82% et 86% de réussite (moins bons que la solution naïve, et bien moins bons que les performances annoncées par M. Bonthron, autour de 94%)*)

open Printf
#use "wordleDictionnaire.ml" 
#use "wordleFonctionsDeBase.ml"
#use "wordleNaive.ml"

#use "topfind";;
#require "owl";; 

let nb_lignes = 26;; 

let () = Printexc.record_backtrace true 


let eigvals_array val_p n =
  let tab = Array.make n 0. in 
  for i = 0 to n - 1 do 
    let r = Owl_dense_matrix_generic.get val_p 0 i in 
    tab.(i) <- r
  done; 
  tab  
;;


let max_of_array tab = 
  let n = Array.length tab in 
  let indice = ref 0 in 
  let elt = ref (float_of_int min_int) in 

  for i = 0 to n - 1 do 
    if tab.(i) > !elt 
      then (
        indice := i ; 
        elt := tab.(i) 
      )
  done;
   !elt, !indice 

;; 


let get_rank_one_approx matrice n = 
  let vect_p, val_p = Owl_linalg.Generic.eig~otyp:Bigarray.float32 matrice in
  let val_p_tab = eigvals_array val_p n in 
  let val_sing, indice = max_of_array val_p_tab in 
  let tableau_de_colonnes = Owl_dense_matrix_generic.to_cols vect_p in 
  let vec_p_associe = tableau_de_colonnes.(indice) in 
  vec_p_associe 
  (*là, le vecteur propre est normalement sous la forme d'une matrice 5*1 *)
;;


let dot_product a b = 
  (*préconditions : deux vecteurs colonnes de même nombre de lignes*)
  let p = ref 0. in 
  let nb_lignes = Owl_dense_matrix_generic.row_num a in 
  for i = 0 to nb_lignes - 1 do 
    let a_i = Owl_dense_matrix_generic.get a i 0 in 
    let b_i = Owl_dense_matrix_generic.get b i 0 in 
    p := !p +. (a_i *. b_i) 
  done; 
  !p 


let get_angle_between a b = 
  let norme_a = Owl_linalg_generic.norm a in (*calcule la norme 2 du vecteur a*)
  let norme_b = Owl_linalg_generic.norm b in 
  let numerateur = dot_product a b in 
  let denominateur = norme_a *. norme_b in 
  let angle_en_radians = Float.acos (numerateur /. denominateur) in 
  let angle_en_degres = (angle_en_radians *. 180.) /. Float.pi in 
  angle_en_degres 
;;



let get_all_the_angles mat vect = 
  let tableau_de_colonnes = Owl_dense_matrix_generic.to_cols mat in
  let nb_mots = Array.length tableau_de_colonnes in 
  let tableau_d_angles = Array.make nb_mots 0. in 
  for i = 0 to nb_mots - 1 do
    let theta_i = get_angle_between tableau_de_colonnes.(i) vect in 
    tableau_d_angles.(i) <- theta_i
  done; 
  tableau_d_angles 



let get_id_closest_vector mat vect = 
  let tab = get_all_the_angles mat vect in 
  let indice = get_min_tab tab in  
  indice





let lettre_de_num j = 
  let debut = int_of_char 'A' in 
  char_of_int (debut + j - 1) 
;; 

let num_de_lettre c = 
  (int_of_char c) - (int_of_char 'A')
;;


(*=============================================================================================================================*)
(*Première version, qui ne tient pas compte de la position des lettres*)

let matrix_of_words_list solutions_possibles = 
  let nb_mots = List.length solutions_possibles in 
  let tab = Array.make_matrix nb_mots nb_lignes 0. in  
  let tab_temp = Array.of_list solutions_possibles in 

  for i = 0 to nb_mots - 1 do 
    for j = 0 to nb_letters - 1 do 
      let indice = num_de_lettre tab_temp.(i).[j] in 
      tab.(i).(indice) <- 1. +. tab.(i).(indice)
    done;  
  done; 
  Owl_dense_matrix_generic.of_arrays Bigarray.float32 tab


let get_best_opener_m () = 
  let m_t = matrix_of_words_list the_old_dict in 
  let m = Owl_dense_matrix_generic.transpose m_t in 
  let big_m = Owl_dense_matrix_generic.dot m m_t in
  let u = get_rank_one_approx big_m nb_lignes in 
  let id = get_id_closest_vector m u in 
  let mot = pick_word_in_dict the_old_dict id in 
  mot 

  
let get_next_play_m solutions_potentielles = 
  let m_t = matrix_of_words_list solutions_potentielles in 
  let m = Owl_dense_matrix_generic.transpose m_t in 
  let big_m = Owl_dense_matrix_generic.dot m m_t in
  let u = get_rank_one_approx big_m nb_lignes in 
  let id = get_id_closest_vector m u in 
  let mot = pick_word_in_dict solutions_potentielles id in 
  mot 

let best_opener = get_best_opener_m () 


(*==========================================================================================================================================*)
(*Deuxième version, qui tient compte de la position*)

let matrix_of_words_list_bis solutions_possibles = 
  let nb_mots = List.length solutions_possibles in 
  let tab = Array.make_matrix nb_mots (nb_lignes*nb_letters) 0. in  
  let tab_temp = Array.of_list solutions_possibles in 

  for i = 0 to nb_mots - 1 do 
    for j = 0 to nb_letters - 1 do 
      let indice = num_de_lettre tab_temp.(i).[j] in (*Numéro (entre 0 et 25) de la j-ème lettre du i-ème mot*)

      tab.(i).(j*26 + indice) <- 1.
    done;  
  done; 
  (* let mat_float = Owl_dense_matrix_generic.of_arrays Bigarray.float32 tab in 
  Owl_dense_matrix_generic.cast_s2c mat_float         *)
  Owl_dense_matrix_generic.of_arrays Bigarray.float32 tab


let get_best_opener_m_bis () = 
  let m_t = matrix_of_words_list_bis the_old_dict in 
  let m = Owl_dense_matrix_generic.transpose m_t in 
  let big_m = Owl_dense_matrix_generic.dot m m_t in
  let u = get_rank_one_approx big_m nb_lignes in 
  (* let v_complex = get_closest_vector m u in 
  let v = Owl_dense_matrix_generic.cast Bigarray.float32 v_complex in 
  let mot = word_of_vect v in  *)
  let id = get_id_closest_vector m u in 
  let mot = pick_word_in_dict the_old_dict id in 
  mot 

  
let get_next_play_m_bis solutions_potentielles = 
  let m_t = matrix_of_words_list_bis solutions_potentielles in 
  let m = Owl_dense_matrix_generic.transpose m_t in 
  let big_m = Owl_dense_matrix_generic.dot m m_t in
  let u = get_rank_one_approx big_m nb_lignes in 
  let id = get_id_closest_vector m u in 
  let mot = pick_word_in_dict solutions_potentielles id in 
  (* let mot = word_of_vect v in  *)
  mot 
  
let best_opener_bis = get_best_opener_m_bis () 


(*===============================================================================================================*)
(*La fonction qui joue les parties*)




exception Found of string * int 
exception Wrong_format
exception Not_member

let wordle_game_matrix () = 
  let secret_indice = Random.int dict_length in 
  let secret_word = pick_word_in_dict the_dict secret_indice in 

  let connaissances = Array.make 256 [] in 
  let solutions_potentielles = ref the_dict in 

try
  for i = 0 to nb_attempts -1 do 


  (*Le joueur joue un coup : il joue le mot le plus proche du mot moyen de la liste*)
  let attempt_player = 
    if i = 0 then best_opener_bis
    else get_next_play_m_bis !solutions_potentielles in

    printf "%s\n" attempt_player; 
    

  (*La machine réagit au coup du joueur*)
  let b, tab = compute_result secret_word attempt_player in 
    if b then raise (Found (attempt_player, (i+1))) 
    else (
      show_result attempt_player tab  
    ) ; 
  
  (*Le joueur interprète la réponse de la machine*)
  actualise_connaissances attempt_player tab connaissances; 
  solutions_potentielles := elague_liste !solutions_potentielles connaissances;

  done; 


  Printf.printf "Le joueur a échoué.\nLe mot à deviner était :\n" ; 
  Printf.printf "%s\n" secret_word ;  
  printf "\n" ;  
  -1 

with 
|Found (solution, numero_tentative) ->
  show_result solution [|1; 1; 1; 1; 1|]; 
  Printf.printf "Le joueur a gagné à la %dième tentative.\n" numero_tentative;  
  numero_tentative 



(*================================================================================================================*)
(*Pour jouer des parties*)


let () = 
Random.self_init () ; 
let debut = Sys.time () in 

printf "the best opener is : %s\n" best_opener ; 
printf "the best opener bis is : %s\n" best_opener_bis; 

let nb_parties = 10_000 in 
let nb_parties_gagnees = ref 0 in 
let somme = ref 0 in 
let nb_coups = Array.init nb_attempts (fun i -> (i, 0)) in 

for i = 0 to nb_parties - 1 do 
  let resultat = wordle_game_matrix () in 
  if (resultat >= 1) && (resultat <= nb_attempts) then (
    let _, n = nb_coups.(resultat -1) in 
    nb_coups.(resultat -1) <- (resultat -1, n + 1) ; 
    nb_parties_gagnees := !nb_parties_gagnees + 1 ; 
    somme := !somme + resultat )
  else
    assert (resultat = -1); 
done; 


let pourcentage_reussite = ((float_of_int !nb_parties_gagnees) /. (float_of_int nb_parties)) *. 100.0 in

let nb_essais_moyen = 
if !nb_parties_gagnees = 0 then 0. else (float_of_int !somme) /. (float_of_int !nb_parties_gagnees) in 

Printf.printf "\nRatio de parties gagnées sur %d parties : %f pourcents\n" nb_parties pourcentage_reussite ; 
Printf.printf "Nombre d'essais moyen en cas de réussite : %f\n" nb_essais_moyen ;

Array.iter (fun (i, n) -> Printf.printf "Nombre de parties gagnées en %d coups : %d\n" (i+1) n) nb_coups ;  

let fin = Sys.time () in 
printf "Durée d'exécution : %f secondes\n" (fin-. debut)