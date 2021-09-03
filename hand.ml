type t = Card.t list

let empty_hand = []

exception EmptyHand

exception CardNotFound

let hand_size = List.length

let add_card hand card = card :: hand

let rec create_hand card_list hand =
  match card_list with
  | [] -> hand
  | h :: t -> create_hand t (add_card hand h)

let remove_card hand card =
  let rec remove_helper head = function
    | h :: t ->
        if h = card then List.append head t
        else remove_helper (List.append head [ h ]) t
    | [] -> raise CardNotFound
  in
  if hand_size hand = 0 then raise EmptyHand else remove_helper [] hand

let list_of_hand hand = hand

let string_of_hand hand =
  let input = list_of_hand hand in

  let rec string_of_hand_helper hand output =
    match hand with
    | [] -> output
    | h :: t ->
        let new_output = output ^ "(" ^ Card.string_of_card h ^ ") " in
        string_of_hand_helper t new_output
  in
  string_of_hand_helper input ""

let card_in_hand hand card = List.mem card hand
