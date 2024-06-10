(*Ce fichier contient la première méthode de résolution du Wordle, ainsi que des fonctions pour élaguer la liste des solutions qui serviront aux autres méthodes*)
(*Sur 10_000 parties, cette méthode a 85% de réussite*)


open WordleDictionnaire 
open WordleFonctionsDeBase 
open Printf


let actualise_connaissances attempt_player resultat connaissances = 

  for i = 1 to nb_letters do 
    let lettre = attempt_player.[i - 1] in 
    let indice = int_of_char lettre in 

    if resultat.(i-1) = 1 then (
      (* connaissances.(indice) <- [i]  *)
      (*Si on ne le savait pas déjà, on dit qu'elle est là *)
      if List.mem 0 connaissances.(indice) (*cas très particulier où le même attempt_player permet de localiser la lettre et de savoir qu'elle n'est nulle part ailleurs, et qu'en plus on commence par la non-occurence (cf photo pour exemple)*) 
        then connaissances.(indice) <- annule_absence_lettre connaissances.(indice) i 
      ; 
      if not (List.mem i connaissances.(indice))
        then connaissances.(indice) <- i::(connaissances.(indice)) (*Elle est -notamment- là *)
    )
    else 
      if resultat.(i-1) = 0 then connaissances.(indice) <- (-i)::(connaissances.(indice)) (*Elle est ailleurs*)
      else (
        (*resultat.(i-1) = -1 *)
        (*Elle n'est nulle part ailleurs que là où on sait *)
        (*On va dire partout ailleurs qu'elle ne doit pas y être*)
        for j = 1 to nb_letters do 

          if (connaissances.(indice) = []) || (not (List.mem j connaissances.(indice))) then 
            connaissances.(indice) <- (-j)::(connaissances.(indice))
        done; 
        (*j'ajoute aussi le zéro pour la fonction must_be_member*)
        connaissances.(indice) <- 0::connaissances.(indice)
      )
      done


  
let must_be_member connaissances = 
  let censees_etre_presentes = ref [] in

  for i = 0 to 225 do 
    if (List.length connaissances.(i) > 0) && (not (List.mem 0 connaissances.(i))) (*Si on sait des choses, et que on n'est pas sûr de l'absence*)
      then
        begin 
          censees_etre_presentes := 
            if List.mem (char_of_int i) !censees_etre_presentes 
            then !censees_etre_presentes 
            else (char_of_int i)::!censees_etre_presentes
        end
  done; 
  !censees_etre_presentes 



let emplacements_connus connaissances = 
  let couples_lettre_emplacement = ref [] in 
  for i = 0 to 255 do 
    List.iter (fun e -> 
      if e > 0 then couples_lettre_emplacement := (char_of_int i, e)::(!couples_lettre_emplacement) )
       connaissances.(i)
  done; 
  !couples_lettre_emplacement 



let garde_si_possible mot connaissances = 
  let possible = ref true in 

  let rec aux l position = 
    match l with 
    | [0] -> possible := false (*permet juste d'accélérer le calcul mais pas nécessaire*)
    | x::_ when (x = -position) -> possible := false
    | _::xs -> aux xs position 
    | _ -> ()
  in 

  (*vérifie qu'une lettre censée être présente est dans le mot :*)
  let censees_etre_presentes =  must_be_member connaissances in 
  List.iter (fun c -> possible := (!possible) && (String.contains mot c) ) censees_etre_presentes ; 

  (*vérifie que les lettres dont on connaît l'emplacement y sont*)
  let couples_lettre_emplacement = emplacements_connus connaissances in 
  List.iter (fun (c,e) -> 
    assert ( (e > 0) && (e <= nb_letters)); 
    possible := (!possible) && (mot.[e-1] = c); 
      ) couples_lettre_emplacement ; 

  for i = 1 to nb_letters do 
    let indice = int_of_char mot.[i - 1] in 
    (*ce qu'on sait sur cette lettre :*)
    let informations = connaissances.(indice) in 
    (*vérifie que les lettres pas censées être là n'y sont effectivement pas*)
    aux informations i  

  done; 

  if !possible then Some mot else None 



let elague_liste liste connaissances = 

  let rec aux a_trier en_cours = 
    match a_trier with 
    | [] -> en_cours
    | x::xs -> match garde_si_possible x connaissances with 
                | None -> aux xs en_cours
                | Some mot -> aux xs (mot::en_cours) 

  in 
  aux liste []




exception Found of string * int 
exception Wrong_format
exception Not_member

let wordle_game_strategie_naive () = 
  let secret_indice = Random.int dict_length in 
  let secret_word = pick_word_in_dict the_dict secret_indice in 

  let connaissances = Array.make 256 [] in 
  let solutions_potentielles = ref the_dict in 

try
  for i = 0 to nb_attempts -1 do 

  (*Le joueur joue un coup*)
  let nb_solutions_potentielles = List.length !solutions_potentielles in 
  let indice = Random.int nb_solutions_potentielles in 
  let attempt_player = pick_word_in_dict !solutions_potentielles indice in 
  Printf.printf "%s\n" attempt_player; 

  (*La machine réagit au coup du joueur*)
  let b, tab = compute_result secret_word attempt_player in 
    if b then raise (Found (attempt_player, (i+1))) 
    else (
      show_result attempt_player tab  
    ); 

  (*Le joueur interprète la réponse de la machine*)
  actualise_connaissances attempt_player tab connaissances;
  solutions_potentielles := elague_liste !solutions_potentielles connaissances; 

  done; 

  Printf.printf "Le joueur a échoué.\nLe mot à deviner était :\n" ; 
  Printf.printf "%s\n" secret_word ; 
  -1 

with 
|Found (solution, numero_tentative) ->
  show_result solution [|1; 1; 1; 1; 1|]; 
  Printf.printf "Le joueur a gagné à la %dième tentative.\n" numero_tentative;
  numero_tentative 


let () = 
Random.self_init () ; (*pour que la graine de l'aléatoire change à chaque fois*)
Printexc.record_backtrace true; 



let nb_parties = 10_000 in 
let nb_parties_gagnees = ref 0 in 
let somme = ref 0 in 
let nb_coups = Array.init nb_attempts (fun i -> (i, 0)) in 

for i = 0 to nb_parties - 1 do 
  let resultat = wordle_game_strategie_naive () in 
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


(*=========================================================================================================*)

(*Fonctions de test*)

let print_connaissances tableau = 
  assert (Array.length tableau = 256) ; 
 

  for i = 65 to 90 do 
    Printf.printf "%c : " (char_of_int i); 
    print_int_list tableau.(i); 
    Printf.printf " | "
  done; 
  Printf.printf "\n"




let teste_actualise_connaissances () = 
  let connaissances = Array.make 256 [] in 

  print_int_list_array connaissances ;
  Printf.printf "\n"; 

  actualise_connaissances "TARIF" [|-1; 0; -1; -1; -1|] connaissances; 
  print_int_list_array connaissances ; 
  Printf.printf "\n"; 

  actualise_connaissances "SOUPE" [|-1; -1; 0; -1; -1|] connaissances; 
  print_int_list_array connaissances ; 
  Printf.printf "\n"; 

  actualise_connaissances "CHAUD" [|-1; -1; 1; 0; 1|] connaissances; 
  print_int_list_array connaissances ; 
  Printf.printf "\n"; 
  
  actualise_connaissances "LUMEN" [|-1; 1; -1; -1; 0|] connaissances; 
  print_int_list_array connaissances ;
  Printf.printf "\n"; 

  actualise_connaissances "QUAND" [| 1; 1; 1; 1; 1|] connaissances; 
  print_int_list_array connaissances; 
  Printf.printf "\n"




  let teste_garde_si_possible_1 () = 
    let connaissances = Array.make 256 [] in 
  
    actualise_connaissances "TARIF" [|-1; 0; -1; -1; -1|] connaissances; 
  
    assert (garde_si_possible "QUAND" connaissances = Some "QUAND"); 
    assert (garde_si_possible "NUAGE" connaissances = Some "NUAGE"); 
    assert (garde_si_possible "CHAUD" connaissances = Some "CHAUD"); 
    assert (garde_si_possible "POSER" connaissances = None); 
    assert (garde_si_possible "CASSE" connaissances = None); 
    assert (garde_si_possible "ALORS" connaissances = None); 
    assert (garde_si_possible "POMME" connaissances = None); 
    Printf.printf "fonction garde_si_possible_1 ok\n"
  
      (*
    possibles :      
    QUAND
    NUAGE 
    CHAUD 
    impossibles : 
    POMME  manque le A
    POSER y'a le R
    CASSE y'a le A au même endroit 
    ALORS y'a le R
    *)
  

let teste_garde_si_possible_2 () = 
  let connaissances = Array.make 256 [] in 
  actualise_connaissances "RASEE" [|0; -1; -1; 1; 0|] connaissances; 
  let _, e1 = compute_result "LIMER" "RASEE" in 
  show_result "RASEE" e1 ; 
  let _, e1 = compute_result "LIMER" "ERRER" in 
  show_result "ERRER" e1 ; 
  assert (garde_si_possible "ERRER" connaissances = Some "ERRER"); 

  actualise_connaissances "ERRER" [|0; 0; 0; 1; 1|] connaissances ;
  let _, e2 = compute_result "LIMER" "ERRER" in 
  show_result "ERRER" e2 ;

  assert (garde_si_possible "ERRER" connaissances = None);

  Printf.printf "fonction garde_si_possible_2 ok\n"


let teste_elague_liste () = 
  let connaissances = Array.make 256 [] in 
  actualise_connaissances "TARIF" [|-1; 0; -1; -1; -1|] connaissances; 

  let liste = "QUAND"::"POMME"::"CASSE"::"NUAGE"::"ALORS"::"CHAUD"::"POSER"::[] in 

  let new_liste = elague_liste liste connaissances in 

  assert (List.mem "QUAND" new_liste); 
  assert (List.mem "NUAGE" new_liste); 
  assert (List.mem "CHAUD" new_liste); 
  assert (List.length new_liste = 3); 

  Printf.printf "fonction elague_liste ok\n"