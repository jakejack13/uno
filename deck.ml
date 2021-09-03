(** The abstract type of values representing decks is a list of cards
    with the head of the list being the top of the deck*)
type t = Card.t list

let empty_deck = []

exception EmptyDeck

(** Array of colors used for create_random_deck *)
let array_of_colors = Card.[| Red; Blue; Green; Yellow |]

(** Array of indicators used for create_random_deck *)
let array_of_indicators =
  Card.
    [|
      One;
      Two;
      Three;
      Four;
      Five;
      Six;
      Seven;
      Eight;
      Nine;
      Zero;
      Skip;
      Reverse;
      PlusTwo;
    |]

let full_deck =
  let deck = ref [] in
  for a = 0 to 1 do
    for i = 0 to Array.length array_of_colors - 1 do
      for j = 0 to Array.length array_of_indicators - 1 do
        deck :=
          Card.create_card array_of_colors.(i) array_of_indicators.(j)
          :: !deck
      done
    done
  done;
  for k = 0 to 3 do
    deck :=
      Card.(
        create_card Wild PlusFour
        :: create_card Wild ChooseColor
        :: !deck)
  done;
  !deck

let deck_size = List.length

let draw_card = function h :: t -> (h, t) | [] -> raise EmptyDeck

let peek_card = function h :: t -> h | [] -> raise EmptyDeck

let add_card_to_bottom deck card = deck @ [ card ]

let add_card_to_top deck card = card :: deck

let rec create_deck card_list deck =
  match card_list with
  | [] -> deck
  | h :: t -> create_deck t (add_card_to_bottom deck h)

(** [random_weights size] is a list of random integers of size [size] *)
let random_weights size =
  Random.self_init ();
  let rec random_helper index list =
    if size <= index then list
    else random_helper (index + 1) (Random.bits () :: list)
  in
  random_helper 0 []

(** [compare weights list a b] is a negative integer when the element at
    the location given by the index of [a] in [list] in [weights] is
    less than the same for [b], positive when vice-versa, and 0 when the
    values are equal *)
let compare weights list a b =
  let rec find x index = function
    | [] -> -1
    | h :: t -> if h = x then index else find x (index + 1) t
  in
  let rec get index current = function
    | [] -> assert false
    | h :: t -> if current = index then h else get index (current + 1) t
  in
  let rfactor =
    Random.self_init ();
    Random.bool ()
  in
  let new_list = if rfactor then List.rev list else list in
  let a_index = find a 0 new_list in
  let b_index = find b 0 new_list in
  get a_index 0 weights - get b_index 0 weights

let shuffle_deck deck =
  let weights = random_weights (List.length deck) in
  List.sort (compare weights deck) deck

let to_list deck = deck
