exception WrongCard

type color =
  | Red
  | Blue
  | Green
  | Yellow
  | None

type direction =
  | Forward
  | Backward

type t = {
  draw_pile : Deck.t ref;
  players_cards : Hand.t array;
  discard_pile : Card.t list;
  current_player : int;
  direction : direction;
  color : color;
  ai_list : bool list;
}

let create_state
    draw_pile
    players_cards
    discard_pile
    current_player
    direction
    color
    ai_list =
  {
    draw_pile;
    players_cards;
    discard_pile;
    current_player;
    direction;
    color;
    ai_list;
  }

let init_state (num_players : int) =
  let new_deck = ref (Deck.shuffle_deck Deck.full_deck) in
  {
    draw_pile = new_deck;
    players_cards = Uno_game.starting_hands num_players new_deck;
    discard_pile = Uno_game.init_discard new_deck;
    current_player = 1;
    direction = Forward;
    color = None;
    ai_list = [ false; false ];
  }

(* Getter functions *)
let get_deck st = st.draw_pile

let current_player st = st.current_player

let current_color st = st.color

let current_direction st = st.direction

let current_discard st = st.discard_pile

let current_players_card st = st.players_cards

let current_hand player_num st = st.players_cards.(player_num - 1)

let rec set_ai_length_helper (ls : bool list) num =
  if num = 0 then ls
  else set_ai_length_helper (List.append ls [ false ]) (num - 1)

let set_player_num (player_num : int) (st : t) =
  {
    draw_pile = st.draw_pile;
    players_cards = st.players_cards;
    discard_pile = st.discard_pile;
    current_player = player_num;
    direction = st.direction;
    color = st.color;
    ai_list = st.ai_list;
  }

let set_ai_length (length : int) (st : t) =
  {
    draw_pile = st.draw_pile;
    players_cards = st.players_cards;
    discard_pile = st.discard_pile;
    current_player = st.current_player;
    direction = st.direction;
    color = st.color;
    ai_list = set_ai_length_helper [] length;
  }

let add_to_player_hand card player_num st =
  let hand = st.players_cards.(player_num - 1) in
  let altered_hand = Hand.add_card hand card in
  let _ = st.players_cards.(player_num - 1) <- altered_hand in
  st

let deck_length st = Deck.deck_size !(st.draw_pile)

let top_card st = List.hd st.discard_pile

let direction_to_string direction =
  match direction with Forward -> "forward" | Backward -> "backward"

let string_to_direction direction =
  if direction = "forward" then Forward else Backward

let string_to_color c =
  if c = "red" then Red
  else if c = "Red" then Red
  else if c = "blue" then Blue
  else if c = "Blue" then Blue
  else if c = "green" then Green
  else if c = "Green" then Green
  else if c = "yellow" then Yellow
  else if c = "Yellow" then Yellow
  else None

let color_to_string = function
  | Red -> "Red"
  | Blue -> "Blue"
  | Green -> "Green"
  | Yellow -> "Yellow"
  | None -> "None"

(* Helper function designed to produce the next player *)
let next_player st =
  match st.direction with
  | Forward ->
      if st.current_player == Array.length st.players_cards then 1
      else st.current_player + 1
  | Backward ->
      if st.current_player == 1 then Array.length st.players_cards
      else st.current_player - 1

(* Checks to see if the card being played both is located in the hand of
   the current player and matches the specification of the top card of
   the draw pile *)
let card_checker card hand st =
  if List.mem card (Hand.list_of_hand hand) == false then false
  else if Card.string_of_color (Card.color_of_card card) = "Wild" then
    true
  else
    match st.color with
    | Red | Green | Yellow | Blue ->
        let card_color =
          Card.string_of_color (Card.color_of_card card)
        in
        if String.equal (color_to_string st.color) card_color then true
        else false
    | None ->
        let top = top_card st in
        let top_color = Card.string_of_color (Card.color_of_card top) in
        let card_color =
          Card.string_of_color (Card.color_of_card card)
        in
        let top_indicator =
          Card.int_of_indicator (Card.indicator_of_card top)
        in
        let card_indicator =
          Card.int_of_indicator (Card.indicator_of_card card)
        in
        if top_indicator != card_indicator then
          if String.equal top_color card_color == false then false
          else true
        else true

(* Outputs the number of the next player when a card with a skip
   function [Skip, PlusTwo, PlusFour] is used. *)
let skip_next (st : t) =
  match st.direction with
  | Forward -> (
      let num =
        (st.current_player + 2) mod Array.length st.players_cards
      in
      match num with 0 -> Array.length st.players_cards | x -> x )
  | Backward -> (
      let num =
        (st.current_player - 2) mod Array.length st.players_cards
      in
      match num with
      | x when x < 0 -> Array.length st.players_cards + x
      | 0 -> Array.length st.players_cards
      | x -> x )

let draw st =
  let new_hand =
    Uno_game.draw st.draw_pile st.players_cards.(st.current_player - 1)
  in
  let p_cards = st.players_cards in
  let _ = p_cards.(st.current_player - 1) <- new_hand in
  {
    draw_pile = st.draw_pile;
    players_cards = p_cards;
    discard_pile = st.discard_pile;
    current_player = next_player st;
    direction = st.direction;
    color = st.color;
    ai_list = st.ai_list;
  }

let play_numbered_card card st =
  let hand = st.players_cards.(st.current_player - 1) in
  if card_checker card hand st == true then
    (* Add card to draw pile here *)
    let _ =
      st.players_cards.(st.current_player - 1) <-
        Uno_game.play_numbered_card card hand
    in
    {
      draw_pile = st.draw_pile;
      players_cards = st.players_cards;
      discard_pile = card :: st.discard_pile;
      current_player = next_player st;
      direction = st.direction;
      color = None;
      ai_list = st.ai_list;
    }
  else raise WrongCard

(* Card isn't in hand *)

let special_draw card st color =
  let hand = st.players_cards.(st.current_player - 1) in
  if card_checker card hand st == true then
    let altered_hand = Hand.remove_card hand card in
    let _ = st.players_cards.(st.current_player - 1) <- altered_hand in
    let hand_with_added_cards = st.players_cards.(next_player st - 1) in
    let _ =
      st.players_cards.(next_player st - 1) <-
        Uno_game.special_draw card hand_with_added_cards st.draw_pile
    in
    if
      String.equal
        (Card.string_of_indicator (Card.indicator_of_card card))
        "PlusFour"
    then
      {
        draw_pile = st.draw_pile;
        players_cards = st.players_cards;
        discard_pile = card :: st.discard_pile;
        current_player = skip_next st;
        direction = st.direction;
        color = string_to_color color;
        ai_list = st.ai_list;
      }
    else
      {
        draw_pile = st.draw_pile;
        players_cards = st.players_cards;
        discard_pile = card :: st.discard_pile;
        current_player = skip_next st;
        direction = st.direction;
        color = None;
        ai_list = st.ai_list;
      }
  else raise WrongCard

let skip card st =
  let hand = st.players_cards.(st.current_player - 1) in
  if card_checker card hand st == true then
    let altered_hand = Uno_game.skip card hand in
    let _ = st.players_cards.(st.current_player - 1) <- altered_hand in
    let new_player = skip_next st in
    {
      draw_pile = st.draw_pile;
      players_cards = st.players_cards;
      discard_pile = card :: st.discard_pile;
      current_player = new_player;
      direction = st.direction;
      color = None;
      ai_list = st.ai_list;
    }
  else raise WrongCard

let choose_color card color st =
  let hand = st.players_cards.(st.current_player - 1) in
  if card_checker card hand st == true then
    let altered_hand = Uno_game.color card hand in
    let _ = st.players_cards.(st.current_player - 1) <- altered_hand in
    let future_color =
      match color with
      | "Red" -> Red
      | "Blue" -> Blue
      | "Yellow" -> Yellow
      | _ -> Green
    in
    {
      draw_pile = st.draw_pile;
      players_cards = st.players_cards;
      discard_pile = card :: st.discard_pile;
      current_player = next_player st;
      direction = st.direction;
      color = future_color;
      ai_list = st.ai_list;
    }
  else raise WrongCard

let reverse card st =
  let hand = st.players_cards.(st.current_player - 1) in
  if card_checker card hand st == true then
    let altered_hand = Uno_game.reverse card hand in
    let _ = st.players_cards.(st.current_player - 1) <- altered_hand in
    let new_direction =
      match st.direction with
      | Forward -> Backward
      | Backward -> Forward
    in
    match st with
    | {
     draw_pile;
     players_cards;
     discard_pile;
     current_player;
     direction;
     color;
    } ->
        let st =
          {
            draw_pile;
            players_cards;
            discard_pile;
            current_player;
            direction = new_direction;
            color;
            ai_list = st.ai_list;
          }
        in
        {
          draw_pile = st.draw_pile;
          players_cards = st.players_cards;
          discard_pile = card :: st.discard_pile;
          current_player = next_player st;
          direction = st.direction;
          color = None;
          ai_list = st.ai_list;
        }
  else raise WrongCard

let rec set_ai_num_helper (ls : bool list) num length counter =
  if num > 0 && counter < length then
    set_ai_num_helper
      (List.append ls [ true ])
      (num - 1) length (counter + 1)
  else if counter < length then
    set_ai_num_helper
      (List.append ls [ false ])
      (num - 1) length (counter + 1)
  else ls

let set_ai_num (ai_num : int) (st : t) =
  {
    draw_pile = st.draw_pile;
    players_cards = st.players_cards;
    discard_pile = st.discard_pile;
    current_player = st.current_player;
    direction = st.direction;
    color = st.color;
    ai_list = set_ai_num_helper [] ai_num (List.length st.ai_list) 0;
  }

let current_ai_list st = st.ai_list
