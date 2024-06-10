(*Contient les fonctions de base pour jouer au wordle*)
(*Appeler wordle_game () permet à l'utilisateur de jouer lui-même une partie*)

open WordleDictionnaire

let nb_letters = 5
let nb_attempts = 6
let the_dict = the_dict
let dict_length = List.length the_dict


let is_same_word w1 w2 = 
  assert (String.length w1 = nb_letters); 
  assert (String.length w2 = nb_letters); 
  let answer = ref true in 

  for i = 0 to nb_letters - 1 do 
    if w1.[i] <> w2.[i] then answer := false
    done; 
  !answer
  

let rec is_member_dict dict w = 
  match dict with 
  | [] -> false 
  | x::xs -> (is_same_word x w) || is_member_dict xs w 



let rec pick_word_in_dict dict indice = 
  match dict with 
  | x::_ when indice = 0 -> x 
  | _::xs -> pick_word_in_dict xs (indice - 1) 
  | _ -> failwith "impossible" 



let contains_letter w c = 
  let answer = ref false in 
  for i = 0 to String.length w -1 do 
    if w.[i] = c then answer := true 
    done; 
    !answer


let gestion_doublons secret_word attempt_player letter =
  (*précondition : il existe au moins un endroit où une lettre présente est placée ailleurs*)
  (*sortie : un booléen qui répond à la question "Reste-t-il d'autres entroits où la lettre apparaît, et qu'on ne connaît pas ?"*)
  let occurences = ref 0 in 
  for i = 0 to nb_letters - 1 do 
    if attempt_player.[i] = letter then occurences := 1 + !occurences  
  done;
  (*S'il n'y a pas de doublon, alors la lettre n'a pas été découverte donc répondre oui *)
  if !occurences <= 1 then true 
  else (
  (*S'il reste un endroit où la lettre n'est pas découverte, alors répondre oui *)
    let b = ref false in 
    for j = 0 to nb_letters - 1 do 
      if secret_word.[j] = letter then (
        if attempt_player.[j] <> letter 
          then b := true 
      )
    done; 
    !b
  ) 


let compute_result secret_word attempt_player = 
  let tab = Array.make nb_letters (-1) in 
  let b = ref true in 
  for i = 0 to nb_letters -1 do 
    if secret_word.[i] <> attempt_player.[i] then (
      b := false ; 
      if contains_letter secret_word attempt_player.[i] 
        then (
          let booleen = gestion_doublons secret_word attempt_player attempt_player.[i] in 
          if booleen then tab.(i) <- 0 (*Dans ce cas, il en reste à trouver*)
          else tab.(i) <- -1 (*Dans ce cas, on a tout trouvé*)
        ) 
      else tab.(i) <- -1   
    )
    else tab.(i) <- 1 
  done; 
  !b, tab  



  let show_result given_word result = 
  (*X correspond au vert, O au jaune et _ à gris*)
    for i = 0 to nb_letters - 1 do 
      if result.(i) = -1 then Printf.printf "_"
      else (
        if result.(i) = 0 then Printf.printf "O"
        else Printf.printf "X"
      ) 
      done; 
      Printf.printf "\n" 



(*La fonction qui permet de jouer les parties : *)

exception Found of string
exception Wrong_format
exception Not_member

let wordle_game () = 
  let secret_indice = Random.int dict_length in 
  let secret_word = pick_word_in_dict the_dict secret_indice in 

try
  for i = 0 to nb_attempts -1 do 

    let attempt_player = read_line () in (*attend une proposition de 5 lettres, en majuscules, en ligne de commande*) 
    if not ((String.length attempt_player) = nb_letters) then raise Wrong_format ; 
    if not (is_member_dict the_dict attempt_player) then raise Not_member ; 

    let b, tab = compute_result secret_word attempt_player in 
    if b then raise (Found attempt_player) 
    else (
      show_result attempt_player tab ; 
    )
  done; 
  Printf.printf "Le joueur a échoué.\nLe mot à deviner était :\n" ; 
  Printf.printf "%s\n" secret_word; 

with 
|Found solution -> 
  (show_result solution [|1; 1; 1; 1; 1|]; 
  Printf.printf "Le joueur a gagné.\n")
| Wrong_format -> Printf.printf "Ce mot n'a pas le nombre de lettres attendu\n"
| Not_member -> Printf.printf "Ce mot n'appartient pas à la liste.\n"



(*=====================================================================================================================================*)



let () = 
(*pour que la graine de l'aléatoire change à chaque fois*)
Printexc.record_backtrace true; 
(* Pour jouer soi-même une partie :*) 
wordle_game () 