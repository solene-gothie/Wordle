(*Ce fichier contient une deuxième méthode de résolution du Wordle, basée sur la théorie de l'information et inspirée des travaux de Grant Sanderson (3B1B) *)
(*Avec cette méthode, sur 10_000 parties on obtient 92% de réussite*)

open WordleDictionnaire
open WordleFonctionsDeBase
open WordleNaive
open Printf 

let () = Printexc.record_backtrace true 


(*Quelques constantes utiles*)
let nb_pattern =  243 (*3⁵*)
let possible_patterns = Array.make_matrix nb_pattern nb_letters 0 
let information_a_connaitre = Float.log2 (float_of_int dict_length)  

(*Résultats obtenus en prétraitement*)
let best_opener = "RAIES"
let max_entropy = 6.2999889676 



(*On remplit le tableau comme un arbre ternaire parcouru en profondeur dans l'ordre préfixe*)
let fill_possible_patterns () = 
  assert (nb_letters = 5); 
  for a = 0 to 2 do 
    for b = 0 to 2 do 
      for c = 0 to 2 do 
        for d = 0 to 2 do 
          for e = 0 to 2 do 
            let position = e + 3*(d + 3*(c + 3*(b + 3*a))) in
            possible_patterns.(position) <- [|a - 1; b - 1; c - 1; d - 1; e - 1|]
          done; 
        done; 
      done; 
    done; 
  done 

let () = 
fill_possible_patterns ()




let are_equal_arrays a b = 
  let reponse = ref true in 
  if Array.length a <> Array.length b then reponse := false 
  else 
    begin 
      for i = 0 to Array.length a - 1 do 
        reponse := !reponse && (a.(i) = b.(i))
      done; 
    end ; 
    !reponse 




let probability_of_pattern word pattern solutions_potentielles = 

  let rec aux liste matches possible_words  = 
    match liste with 
    | [] -> let m_float = float_of_int matches in (*nb de solutions pour lesquelles le mot donné donne le motif donné*)
            let nb_pat_float = float_of_int (List.length solutions_potentielles) in (*nb de solutions*)
            (m_float /. nb_pat_float), possible_words
    | x::xs -> 
      let _ , result = compute_result x word in (*Si la solution était x, que répondrait la machine ? *)
      if are_equal_arrays result pattern 
        then aux xs (matches + 1) (x::possible_words) 
      else aux xs matches possible_words 
    in
  aux solutions_potentielles 0 [] 



let entropy word solutions_potentielles= 
  let ent = ref 0. in 

  for i = 0 to nb_pattern -1 do 
    let (p, _) = (probability_of_pattern word possible_patterns.(i) solutions_potentielles) in 
    if p > 0. then ent := !ent +. p*.(Float.log2 p) 
  done; 

  -. !ent 



let get_next_play possible_words  = 

let rec aux liste max_ent word = 
  match liste with 
  |[] -> word, max_ent 
  |x::xs -> 
    let ent_of_x = entropy x possible_words in 
    if ent_of_x > max_ent then 
      aux xs ent_of_x x 
    else 
      aux xs max_ent word 
in 

match possible_words with 
| [] -> failwith "Il n'y a pas de solutions verifiant les contraintes ; impossible"
| x::xs -> aux xs (entropy x possible_words) x 



(*=======================================================================================================================*)
(*Fonctions de prétraitement ayant permis d'obtenir le premier coup à jouer *)


let get_entropy words solutions_potentielles= 
  let rec aux  words words_and_ent = 
  match words with 
  | [] -> words_and_ent 
  | x::xs -> 
    let ent = entropy x solutions_potentielles in 
    aux xs ((x, ent)::words_and_ent)
  in 
  aux words []



let rec show_entropies liste = 
  match liste with 
  | [] -> ()
  | (word, ent)::xs -> printf "%s : %f\n" word ent ; show_entropies xs 

(*========================================================================================================================*)




exception Found of string * int 
exception Wrong_format
exception Not_member

let wordle_game_shannon_v1 () = 
  let secret_indice = Random.int dict_length in 
  let secret_word = pick_word_in_dict the_dict secret_indice in 

  let connaissances = Array.make 256 [] in 
  let solutions_potentielles = ref the_dict in 
  let incertitude = ref information_a_connaitre in 

try
  for i = 0 to nb_attempts -1 do 

  (*Le joueur joue un coup : il joue le mot maximisant l'espérance en gain d'information*)
  let attempt_player, esperance_gain_info = 
    if i = 0 then best_opener, max_entropy
    else 
      get_next_play !solutions_potentielles 
    in 
  printf "%s : %f\n" attempt_player esperance_gain_info ;  

  (*La machine réagit au coup du joueur*)
  let b, tab = compute_result secret_word attempt_player in 
    if b then raise (Found (attempt_player, (i+1))) 
    else (
      show_result attempt_player tab  
    )

  (*Le joueur interprète la réponse de la machine*)
  let (p, _) = probability_of_pattern attempt_player tab !solutions_potentielles  in 
  let gain_info_reel = -. Float.log2 p in 
  printf "gain info reel:  %f\n" gain_info_reel ;   
  incertitude := !incertitude -. gain_info_reel ; 


  actualise_connaissances attempt_player tab connaissances; 
  solutions_potentielles := elague_liste !solutions_potentielles connaissances;

  printf "nb solutions potentielles : %d\n" (List.length !solutions_potentielles) ; 
  printf "incertitude restante : %f\n" !incertitude ; 

  (*Manière alternative d'exprimer l'incertitude restante :*)
  (* let nb_restantes = float_of_int (List.length !solutions_potentielles) in 
  let reste_a_savoir = (Float.log2 nb_restantes) in 
  printf "reste à savoir : %f\n" reste_a_savoir ;   *)
  

  done; 

  Printf.printf "Le joueur a échoué.\nLe mot à deviner était :\n" ; 
  Printf.printf "%s\n\n" secret_word ;   
  -1 

with 
|Found (solution, numero_tentative) ->
  show_result solution [|1; 1; 1; 1; 1|]; 
  Printf.printf "Le joueur a gagné à la %dième tentative.\n\n" numero_tentative; 
  numero_tentative 



(*===============================================================================================================*)


let () = 
Random.self_init () ; 
let debut = Sys.time () in 

(*indiquer le nombre de parties qu'on veut jouer :*)
let nb_parties = 2000 in 
let nb_parties_gagnees = ref 0 in 
let somme = ref 0 in 
let nb_coups = Array.init nb_attempts (fun i -> (i, 0)) in 

for i = 0 to nb_parties - 1 do 
  let resultat = wordle_game_shannon_v1 () in 
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
