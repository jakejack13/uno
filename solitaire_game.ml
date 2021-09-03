exception InvalidMove

(* Return type shoudl be Card.t list*)
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

let starting_hand_player deck = cards_draw deck 12 Hand.empty_hand

let is_numbered_card (card : Card.t) =
  let i = Card.indicator_of_card card in
  if Card.int_of_indicator i > -1 then true else false

(*Doesn't ensure that the dealer gets four cards 1-9 *)
let hand_dealer (deck : Deck.t ref) =
  deck := Deck.shuffle_deck !deck;
  let rec hand_aux hand num =
    if num = 0 then hand
    else
      let card, new_deck = Deck.draw_card !deck in
      if card |> Card.indicator_of_card |> Card.int_of_indicator > 0
      then (
        deck := new_deck;
        hand_aux (Hand.add_card hand card) (num - 1) )
      else (
        deck := Deck.add_card_to_bottom new_deck card;
        hand_aux hand num )
  in
  hand_aux Hand.empty_hand 4

let match_card
    (card : Card.t)
    (dealer_card : int)
    (dealer_hand : Hand.t) =
  Random.self_init ();
  if card = List.nth (Hand.list_of_hand dealer_hand) (dealer_card - 1)
  then 0
  else
    (card |> Card.indicator_of_card |> Card.int_of_indicator)
    - ( List.nth (Hand.list_of_hand dealer_hand) (dealer_card - 1)
      |> Card.indicator_of_card |> Card.int_of_indicator )

let draw (deck : Deck.t ref) (hand : Hand.t) = cards_draw deck 1 hand

let special_draw
    (card : Card.t)
    (player_cards : Hand.t)
    (deck : Deck.t ref) =
  match Card.indicator_of_card card with
  | PlusTwo -> cards_draw deck 2 player_cards
  | PlusFour -> cards_draw deck 4 player_cards
  | _ -> raise InvalidMove

let special_skip card1 card2 dealer_num dealer_hand =
  match_card card2 dealer_num dealer_hand

let special_color card dealer_hand dealer_num =
  List.nth (dealer_hand |> Hand.list_of_hand) (dealer_num - 1)
  |> Card.color_of_card

let special_reverse card dealer_num guess dealer_cards =
  List.nth (dealer_cards |> Hand.list_of_hand) (dealer_num - 1)
  |> Card.indicator_of_card |> Card.int_of_indicator = guess
