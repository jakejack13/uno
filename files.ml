let solitaire_save (st : Solitaire_state.t) (name : string) =
  let output = open_out (name ^ ".json") in
  let deck = Solitaire_state.get_deck st in
  let deck_list = Deck.to_list !deck in
  let player_hand = Solitaire_state.current_hand st in
  let dealer_hand = Solitaire_state.dealer_hand st in
  let player_list = Hand.list_of_hand player_hand in
  let dealer_list = Hand.list_of_hand dealer_hand in
  let rec output_loop (input : Card.t list) (output_string : string) =
    match input with
    | [] -> output_string
    | h :: t ->
        let split_string =
          String.split_on_char ' ' (Card.string_of_card h)
        in
        output_loop t "\n\t\t{\n\t\t\t\"color\": "
        ^ "\"" ^ List.hd split_string ^ "\","
        ^ "\n\t\t\t\"indicator\": " ^ "\""
        ^ List.hd (List.tl split_string)
        ^ "\"" ^ "\n\t\t}" ^ "," ^ output_string
    (* let split_string = String.split_on_char ' ' (Card.string_of_card
       h) in output_loop t "\n\t\t{\n\t\t\t\"color\": " ^ output_string
       ^ "\"" ^ List.hd split_string ^ "\"," ^ "\n\t\t\t\"indicator\": "
       ^ "\"" ^ List.hd (List.tl split_string) ^ "\"" ^ "\n\t\t}" ^ "," *)
  in
  Printf.fprintf output "{";
  Printf.fprintf output "\n\t\"Game\": \"Solitaire\",";
  Printf.fprintf output "\n\t\"Deck\": ";
  Printf.fprintf output "[";
  let deck_outputted = output_loop deck_list "" in
  let deck_formatted =
    String.sub deck_outputted 0 (String.length deck_outputted - 1)
  in
  Printf.fprintf output "%s" deck_formatted;
  Printf.fprintf output " ],";
  Printf.fprintf output "\n\t\"Player_cards\": ";
  Printf.fprintf output "[";
  let player_outputted = output_loop player_list "" in
  let player_formatted =
    String.sub player_outputted 0 (String.length player_outputted - 1)
  in
  Printf.fprintf output "%s" player_formatted;
  Printf.fprintf output " ],";
  Printf.fprintf output "\n\t\"Dealer_cards\": ";
  Printf.fprintf output "[";
  let dealer_outputted = output_loop dealer_list "" in
  let dealer_formatted =
    String.sub dealer_outputted 0 (String.length dealer_outputted - 1)
  in
  Printf.fprintf output "%s" dealer_formatted;
  Printf.fprintf output " ],";
  Printf.fprintf output "\n\t\"Turn_count\": \"";
  Printf.fprintf output "%i" (Solitaire_state.turns st);
  Printf.fprintf output "\"";
  Printf.fprintf output "\n}";
  close_out output

let solitaire_load (name : string) =
  let input = name ^ ".json" in
  let rec cards_rec cards =
    match cards with
    | [] -> []
    | h :: t ->
        let color =
          Yojson.Basic.Util.to_string
            (List.assoc "color" (Yojson.Basic.Util.to_assoc h))
        in
        let indicator =
          Yojson.Basic.Util.to_string
            (List.assoc "indicator" (Yojson.Basic.Util.to_assoc h))
        in
        let output =
          Card.create_card
            (Card.color_of_string color)
            (Card.indicator_of_string indicator)
        in
        output :: cards_rec t
  in
  let json = Yojson.Basic.from_file input in
  let turns =
    int_of_string
      (Yojson.Basic.Util.to_string
         (List.assoc "Turn_count" (Yojson.Basic.Util.to_assoc json)))
  in
  let player_cards =
    cards_rec
      (Yojson.Basic.Util.to_list
         (List.assoc "Player_cards" (Yojson.Basic.Util.to_assoc json)))
  in
  let dealer_cards =
    cards_rec
      (Yojson.Basic.Util.to_list
         (List.assoc "Dealer_cards" (Yojson.Basic.Util.to_assoc json)))
  in
  let deck =
    cards_rec
      (Yojson.Basic.Util.to_list
         (List.assoc "Deck" (Yojson.Basic.Util.to_assoc json)))
  in
  let ref_deck = Deck.create_deck deck Deck.empty_deck in
  Solitaire_state.create_state (ref ref_deck)
    (Hand.create_hand player_cards Hand.empty_hand)
    (Hand.create_hand dealer_cards Hand.empty_hand)
    turns

let uno_save (st : Uno_state.t) (name : string) =
  let output = open_out (name ^ ".json") in
  let deck = Uno_state.get_deck st in
  let deck_list = Deck.to_list !deck in
  let players_card = Uno_state.current_players_card st in
  let players_card_zero = players_card.(0) in
  let players_card_one = players_card.(1) in
  let players_card_two = players_card.(2) in
  let players_card_three = players_card.(3) in
  let players_card_zero_list = Hand.list_of_hand players_card_zero in
  let players_card_one_list = Hand.list_of_hand players_card_one in
  let players_card_two_list = Hand.list_of_hand players_card_two in
  let players_card_three_list = Hand.list_of_hand players_card_three in
  let discard_pile = Uno_state.current_discard st in
  let ai_list = Uno_state.current_ai_list st in
  let rec output_loop (input : Card.t list) (output_string : string) =
    match input with
    | [] -> output_string
    | h :: t ->
        let split_string =
          String.split_on_char ' ' (Card.string_of_card h)
        in
        output_loop t "\n\t\t{\n\t\t\t\"color\": "
        ^ "\"" ^ List.hd split_string ^ "\","
        ^ "\n\t\t\t\"indicator\": " ^ "\""
        ^ List.hd (List.tl split_string)
        ^ "\"" ^ "\n\t\t}" ^ "," ^ output_string
    (* let split_string = String.split_on_char ' ' (Card.string_of_card
       h) in output_loop t "\n\t\t{\n\t\t\t\"color\": " ^ output_string
       ^ "\"" ^ List.hd split_string ^ "\"," ^ "\n\t\t\t\"indicator\": "
       ^ "\"" ^ List.hd (List.tl split_string) ^ "\"" ^ "\n\t\t}" ^ "," *)
  in

  let rec ai_loop (input : bool list) (output_string : string) =
    match input with
    | [] -> output_string
    | h :: t ->
        ai_loop t (output_string ^ "\"" ^ string_of_bool h ^ "\"" ^ ",")
  in

  Printf.fprintf output "{";
  Printf.fprintf output "\n\t\"Game\": \"Uno\",";
  Printf.fprintf output "\n\t\"Deck\": ";
  Printf.fprintf output "[";
  let deck_outputted = output_loop deck_list "" in
  let deck_formatted =
    String.sub deck_outputted 0 (String.length deck_outputted - 1)
  in
  Printf.fprintf output "%s" deck_formatted;
  Printf.fprintf output " ],";
  let player_zero_outputted = output_loop players_card_zero_list "" in
  let player_one_outputted = output_loop players_card_one_list "" in
  let player_two_outputted = output_loop players_card_two_list "" in
  let player_three_outputted = output_loop players_card_three_list "" in
  let player_zero_formatted =
    String.sub player_zero_outputted 0
      (String.length player_zero_outputted - 1)
  in
  let player_one_formatted =
    String.sub player_one_outputted 0
      (String.length player_one_outputted - 1)
  in
  let player_two_formatted =
    String.sub player_two_outputted 0
      (String.length player_two_outputted - 1)
  in
  let player_three_formatted =
    String.sub player_three_outputted 0
      (String.length player_three_outputted - 1)
  in
  let discard_outputted = output_loop discard_pile "" in
  let discard_formatted =
    String.sub discard_outputted 0 (String.length discard_outputted - 1)
  in
  Printf.fprintf output "\n\t\"Player_zero\": ";
  Printf.fprintf output "[";
  Printf.fprintf output "%s" player_zero_formatted;
  Printf.fprintf output " ],";
  Printf.fprintf output "\n\t\"Player_one\": ";
  Printf.fprintf output "[";
  Printf.fprintf output "%s" player_one_formatted;
  Printf.fprintf output " ],";
  Printf.fprintf output "\n\t\"Player_two\": ";
  Printf.fprintf output "[";
  Printf.fprintf output "%s" player_two_formatted;
  Printf.fprintf output " ],";
  Printf.fprintf output "\n\t\"Player_three\": ";
  Printf.fprintf output "[";
  Printf.fprintf output "%s" player_three_formatted;
  Printf.fprintf output " ],";
  Printf.fprintf output "\n\t\"Discard_pile\": ";
  Printf.fprintf output "[";
  Printf.fprintf output "%s" discard_formatted;
  Printf.fprintf output " ],";

  Printf.fprintf output "\n\t\"Current_Player\": \"";
  Printf.fprintf output "%i" (Uno_state.current_player st);
  Printf.fprintf output "\",";
  Printf.fprintf output "\n";
  Printf.fprintf output "\n\t\"Direction\": \"";
  Printf.fprintf output "%s"
    (Uno_state.direction_to_string (Uno_state.current_direction st));
  Printf.fprintf output "\",";
  Printf.fprintf output "\n";
  Printf.fprintf output "\n\t\"Color\": \"";
  Printf.fprintf output "%s"
    (Uno_state.color_to_string (Uno_state.current_color st));
  Printf.fprintf output "\",";

  Printf.fprintf output "\n";
  Printf.fprintf output "\n\t\"Ai_List\": ";
  Printf.fprintf output "[";
  let output_ai_loop = ai_loop ai_list "" in
  let formatted_ai_loop =
    String.sub output_ai_loop 0 (String.length output_ai_loop - 1)
  in
  Printf.fprintf output "%s" formatted_ai_loop;
  Printf.fprintf output " ]";
  Printf.fprintf output "\n}";

  close_out output

let uno_load (name : string) =
  let input = name ^ ".json" in
  let rec cards_rec cards =
    match cards with
    | [] -> []
    | h :: t ->
        let color =
          Yojson.Basic.Util.to_string
            (List.assoc "color" (Yojson.Basic.Util.to_assoc h))
        in
        let indicator =
          Yojson.Basic.Util.to_string
            (List.assoc "indicator" (Yojson.Basic.Util.to_assoc h))
        in
        let output =
          Card.create_card
            (Card.color_of_string color)
            (Card.indicator_of_string indicator)
        in
        output :: cards_rec t
  in
  let rec ai_rec ls (outputter : bool list) =
    match ls with
    | [] -> outputter
    | h :: t ->
        ai_rec t
          (List.append outputter
             [ bool_of_string (Yojson.Basic.Util.to_string h) ])
  in

  let json = Yojson.Basic.from_file input in
  let current_player =
    int_of_string
      (Yojson.Basic.Util.to_string
         (List.assoc "Current_Player" (Yojson.Basic.Util.to_assoc json)))
  in

  let direction =
    Uno_state.string_to_direction
      (Yojson.Basic.Util.to_string
         (List.assoc "Direction" (Yojson.Basic.Util.to_assoc json)))
  in

  let color =
    Uno_state.string_to_color
      (Yojson.Basic.Util.to_string
         (List.assoc "Color" (Yojson.Basic.Util.to_assoc json)))
  in

  let discard_pile =
    cards_rec
      (Yojson.Basic.Util.to_list
         (List.assoc "Discard_pile" (Yojson.Basic.Util.to_assoc json)))
  in
  let ai_list_yo_json =
    Yojson.Basic.Util.to_list
      (List.assoc "Ai_List" (Yojson.Basic.Util.to_assoc json))
  in
  let ai_list_no_json = ai_rec ai_list_yo_json [] in
  let hand_list_zero =
    Yojson.Basic.Util.to_list
      (List.assoc "Player_zero" (Yojson.Basic.Util.to_assoc json))
  in
  let hand_list_one =
    Yojson.Basic.Util.to_list
      (List.assoc "Player_one" (Yojson.Basic.Util.to_assoc json))
  in
  let hand_list_two =
    Yojson.Basic.Util.to_list
      (List.assoc "Player_two" (Yojson.Basic.Util.to_assoc json))
  in
  let hand_list_three =
    Yojson.Basic.Util.to_list
      (List.assoc "Player_three" (Yojson.Basic.Util.to_assoc json))
  in
  let hand_list_zero_cards = cards_rec hand_list_zero in
  let hand_list_one_cards = cards_rec hand_list_one in
  let hand_list_two_cards = cards_rec hand_list_two in
  let hand_list_three_cards = cards_rec hand_list_three in
  let hand_array =
    [|
      Hand.create_hand hand_list_zero_cards Hand.empty_hand;
      Hand.create_hand hand_list_one_cards Hand.empty_hand;
      Hand.create_hand hand_list_two_cards Hand.empty_hand;
      Hand.create_hand hand_list_three_cards Hand.empty_hand;
    |]
  in
  let deck =
    cards_rec
      (Yojson.Basic.Util.to_list
         (List.assoc "Deck" (Yojson.Basic.Util.to_assoc json)))
  in
  let ref_deck = Deck.create_deck deck Deck.empty_deck in
  (*needs changed to an actual ai_list*)
  Uno_state.create_state (ref ref_deck) hand_array discard_pile
    current_player direction color ai_list_no_json
