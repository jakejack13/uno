exception InvalidMove

(* INITIALIZATION FUNCTIONS *)
let rec cards_draw
    (deck : Deck.t ref)
    (num_cards : int)
    (player_cards : Hand.t) =
  if num_cards == 0 then player_cards
  else
    (* Use deck functions *)
    let card, new_deck = Deck.draw_card !deck in
    deck := new_deck;
    cards_draw deck (num_cards - 1) (Hand.add_card player_cards card)

let starting_hands (num_players : int) (deck : Deck.t ref) =
  let array_hands = Array.make num_players Hand.empty_hand in
  let rec card_matcher num acc =
    match num with
    | 1 ->
        let _ = acc.(0) <- cards_draw deck 7 Hand.empty_hand in
        acc
    | n ->
        let _ = acc.(n - 1) <- cards_draw deck 7 Hand.empty_hand in
        card_matcher (num - 1) acc
  in
  card_matcher num_players array_hands

let init_discard deck =
  let temp_hand = cards_draw deck 1 Hand.empty_hand in
  match Hand.list_of_hand temp_hand with
  | [] -> raise Not_found
  | h :: t -> [ h ]

(* DRAWING FUNCTIONS *)

let draw (deck : Deck.t ref) (hand : Hand.t) = cards_draw deck 1 hand

let special_draw
    (card : Card.t)
    (player_cards : Hand.t)
    (deck : Deck.t ref) =
  match Card.indicator_of_card card with
  | PlusTwo -> cards_draw deck 2 player_cards
  | PlusFour -> cards_draw deck 4 player_cards
  | _ -> raise InvalidMove

(* CARD PLAY FUNCTIONS *)

let play_numbered_card card (hand : Hand.t) = Hand.remove_card hand card

let skip card hand = Hand.remove_card hand card

let color card hand = Hand.remove_card hand card

let reverse card hand = Hand.remove_card hand card

let is_numbered_card card =
  match Card.int_of_indicator (Card.indicator_of_card card) with
  | -1 -> false
  | _ -> true

let easy_ai_play (hand : Hand.t) (card : Card.t) =
  let card_color = Card.color_of_card card in
  let card_indicator = Card.indicator_of_card card in
  let hand_list = Hand.list_of_hand hand in
  let rec ai_play_helper hand_list_help =
    match hand_list_help with
    | [] -> Command.Draw
    | h :: t ->
        if
          Card.color_of_card h = card_color
          || Card.indicator_of_card h = card_indicator
          || Card.color_of_card h = Card.Wild
        then
          let return_card =
            Card.create_card (Card.color_of_card h)
              (Card.indicator_of_card h)
          in
          Command.Play return_card
        else ai_play_helper t
  in
  ai_play_helper hand_list

let medium_ai_play (hand : Hand.t) (card : Card.t) =
  let card_color = Card.color_of_card card in
  let card_indicator = Card.indicator_of_card card in
  let hand_list = Hand.list_of_hand hand in
  let rec ai_play_helper_one hand_list_help untouched_hand_list =
    match hand_list_help with
    | [] ->
        let rec ai_player_helper_two
            hand_list_help_two
            untouched_hand_list =
          match hand_list_help_two with
          | [] -> Command.Draw
          | h :: t ->
              if
                Card.color_of_card h = card_color
                || Card.color_of_card h = Card.Wild
              then
                let return_card =
                  Card.create_card (Card.color_of_card h)
                    (Card.indicator_of_card h)
                in
                Command.Play return_card
              else ai_player_helper_two t untouched_hand_list
        in
        ai_player_helper_two untouched_hand_list untouched_hand_list
    | h :: t ->
        if Card.indicator_of_card h = card_indicator then
          let return_card =
            Card.create_card (Card.color_of_card h)
              (Card.indicator_of_card h)
          in
          Command.Play return_card
        else ai_play_helper_one t untouched_hand_list
  in
  ai_play_helper_one hand_list hand_list

let hard_ai_play (hand : Hand.t) (card : Card.t) =
  let card_color = Card.color_of_card card in
  let card_indicator = Card.indicator_of_card card in
  let hand_list = Hand.list_of_hand hand in
  let rec ai_play_helper_one hand_list_help untouched_hand_list =
    match hand_list_help with
    | [] ->
        let rec ai_player_helper_two
            hand_list_help_two
            untouched_hand_list =
          match hand_list_help_two with
          | [] ->
              let rec ai_player_helper_three
                  hand_list_help_three
                  untouched_hand_list =
                match untouched_hand_list with
                | [] -> Command.Draw
                | h :: t ->
                    if Card.color_of_card h = Card.Wild then
                      let return_card =
                        Card.create_card (Card.color_of_card h)
                          (Card.indicator_of_card h)
                      in
                      Command.Play return_card
                    else Command.Draw
              in
              ai_player_helper_three untouched_hand_list
                untouched_hand_list
          | h :: t ->
              if Card.color_of_card h = card_color then
                let return_card =
                  Card.create_card (Card.color_of_card h)
                    (Card.indicator_of_card h)
                in
                Command.Play return_card
              else ai_player_helper_two t untouched_hand_list
        in
        ai_player_helper_two untouched_hand_list untouched_hand_list
    | h :: t ->
        if Card.indicator_of_card h = card_indicator then
          let return_card =
            Card.create_card (Card.color_of_card h)
              (Card.indicator_of_card h)
          in
          Command.Play return_card
        else ai_play_helper_one t untouched_hand_list
  in
  ai_play_helper_one hand_list hand_list

let easy_ai_choose_color =
  Random.self_init ();
  let random_num = Random.int 4 in
  if random_num = 0 then Card.Red
  else if random_num = 1 then Card.Blue
  else if random_num = 2 then Card.Yellow
  else Card.Green

let rec medium_ai_choose_color_helper hand =
  match hand with
  | [] -> Card.Red
  | h :: t ->
      if Card.color_of_card h != Card.Wild then Card.color_of_card h
      else medium_ai_choose_color_helper t

let rec medium_ai_choose_color hand =
  medium_ai_choose_color_helper (Hand.list_of_hand hand)

let rec hard_ai_choose_color_helper hand red blue yellow green =
  match hand with
  | [] ->
      let max_value = ref red in
      if !max_value > blue then max_value := !max_value
      else max_value := blue;
      if !max_value > yellow then max_value := !max_value
      else max_value := yellow;
      if !max_value > green then max_value := !max_value
      else max_value := green;
      let top_color = !max_value in
      if top_color = red then Card.Red
      else if top_color = blue then Card.Blue
      else if top_color = yellow then Card.Yellow
      else Card.Green
  | h :: t ->
      if Card.color_of_card h = Red then
        hard_ai_choose_color_helper t (red + 1) blue yellow green
      else if Card.color_of_card h = Blue then
        hard_ai_choose_color_helper t red (blue + 1) yellow green
      else if Card.color_of_card h = Yellow then
        hard_ai_choose_color_helper t red blue (yellow + 1) green
      else if Card.color_of_card h = Green then
        hard_ai_choose_color_helper t red blue yellow (green + 1)
      else hard_ai_choose_color_helper t red blue yellow green

let hard_ai_choose_color hand =
  hard_ai_choose_color_helper (Hand.list_of_hand hand) 0 0 0 0

let ai_choose_color_all hand difficulty =
  Command.(
    match difficulty with
    | Easy -> easy_ai_choose_color
    | Medium -> medium_ai_choose_color hand
    | Hard -> hard_ai_choose_color hand)

let ai_play_all hand card difficulty =
  Command.(
    match difficulty with
    | Easy -> easy_ai_play hand card
    | Medium -> medium_ai_play hand card
    | Hard -> hard_ai_play hand card)
