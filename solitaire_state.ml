type t = {
  deck : Deck.t ref;
  player_cards : Hand.t;
  dealer_cards : Hand.t;
  turn_count : int;
}

let create_state deck player_cards dealer_cards turn_count =
  { deck; player_cards; dealer_cards; turn_count }

let get_deck st = st.deck

let init_state =
  let new_deck = ref (Deck.shuffle_deck Deck.full_deck) in
  {
    deck = new_deck;
    dealer_cards = Solitaire_game.hand_dealer new_deck;
    player_cards = Solitaire_game.starting_hand_player new_deck;
    turn_count = 0;
  }

let current_hand st = st.player_cards

let dealer_hand st = st.dealer_cards

let turns st = st.turn_count

let deck_length st =
  let deck = st.deck in
  Deck.deck_size !deck

let dealer_length st = Hand.hand_size st.dealer_cards

let turn_count_update st = { st with turn_count = st.turn_count + 1 }

let is_card_in_hand card st = Hand.card_in_hand st.player_cards card

let remove_card card dealer_cards = Hand.remove_card dealer_cards card

let get_card_num num st =
  List.nth (st.dealer_cards |> Hand.list_of_hand) num
  |> Card.indicator_of_card |> Card.int_of_indicator

let match_card card card_num st =
  let st = turn_count_update st in
  if not (is_card_in_hand card st) then raise Not_found
  else
    let new_player_cards = remove_card card st.player_cards in
    let match_num =
      Solitaire_game.match_card card card_num st.dealer_cards
    in
    if match_num = 0 then
      let dealer_cards =
        remove_card
          (List.nth
             (st.dealer_cards |> Hand.list_of_hand)
             (card_num - 1))
          st.dealer_cards
      in
      ( {
          deck = st.deck;
          player_cards = new_player_cards;
          dealer_cards;
          turn_count = st.turn_count;
        },
        match_num )
    else
      ( {
          deck = st.deck;
          player_cards = st.player_cards;
          dealer_cards = st.dealer_cards;
          turn_count = st.turn_count;
        },
        match_num )

let draw st =
  let st = turn_count_update st in
  {
    deck = st.deck;
    player_cards = Solitaire_game.draw st.deck st.player_cards;
    dealer_cards = st.dealer_cards;
    turn_count = st.turn_count;
  }

let special_draw card st =
  if not (is_card_in_hand card st) then raise Not_found
  else
    let st = turn_count_update st in
    let new_player_cards = remove_card card st.player_cards in
    {
      deck = st.deck;
      player_cards =
        Solitaire_game.special_draw card new_player_cards st.deck;
      dealer_cards = st.dealer_cards;
      turn_count = st.turn_count;
    }

let special_skip card1 card2 (card_num : int) st =
  if (not (is_card_in_hand card1 st)) || not (is_card_in_hand card2 st)
  then raise Not_found
  else
    let player_cards = remove_card card1 st.player_cards in
    let player_cards_final = remove_card card2 player_cards in
    let num =
      Solitaire_game.special_skip card1 card2 card_num st.dealer_cards
    in
    if num = 0 then
      let new_dealer_cards =
        remove_card
          (List.nth
             (st.dealer_cards |> Hand.list_of_hand)
             (card_num - 1))
          st.dealer_cards
      in
      ( {
          deck = st.deck;
          player_cards = player_cards_final;
          dealer_cards = new_dealer_cards;
          turn_count = st.turn_count;
        },
        num )
    else
      ( {
          deck = st.deck;
          player_cards = player_cards_final;
          dealer_cards = st.dealer_cards;
          turn_count = st.turn_count;
        },
        num )

let special_color card card_num st =
  if not (is_card_in_hand card st) then raise Not_found
  else
    let new_player_cards = remove_card card st.player_cards in
    let turn_count = turn_count_update st in
    ( {
        deck = turn_count.deck;
        player_cards = new_player_cards;
        dealer_cards = turn_count.dealer_cards;
        turn_count = turn_count.turn_count;
      },
      Solitaire_game.special_color card st.dealer_cards card_num )

let special_reverse card dealer_num guess st =
  if not (is_card_in_hand card st) then raise Not_found
  else
    let new_player_cards = remove_card card st.player_cards in
    let turn_count = turn_count_update st in
    ( {
        deck = turn_count.deck;
        player_cards = new_player_cards;
        dealer_cards = turn_count.dealer_cards;
        turn_count = turn_count.turn_count;
      },
      Solitaire_game.special_reverse card dealer_num guess
        st.dealer_cards )
