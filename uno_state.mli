(** The abstract type of values representing the game state *)
type t

type color

type direction

exception WrongCard

(* Initialization of state + testing *)

(** [create state] allows testing of state methods by sending in inputs
    to make a state with specified values*)
val create_state :
  Deck.t ref ->
  Hand.t array ->
  Card.t list ->
  int ->
  direction ->
  color ->
  bool list ->
  t

(** [init state] The initial state of the game when playing uno. In this
    state, the deck is full apart from the cards that have been
    distributed to the players. Takes in an int representing the number
    of players in the game. Randomizes the first turn. *)
val init_state : int -> t

(* Getters *)

(** [get_deck] returns the deck ref of the inputted state*)
val get_deck : t -> Deck.t ref

(** The list of cards that the specified player currently has *)
val current_hand : int -> t -> Hand.t

(** Returns the player that needs to play their card next *)
val current_player : t -> int

(** Returns the state's color *)
val current_color : t -> color

(** Returns the state's direction *)
val current_direction : t -> direction

(** Returns the state's discard *)
val current_discard : t -> Card.t list

(** Returns the state's current_players *)
val current_players_card : t -> Hand.t array

(** Returns the state's ai_list *)
val current_ai_list : t -> bool list

(** Changes the player num *)
val set_player_num : int -> t -> t

(** Adds a card to the given player's hand*)
val add_to_player_hand : Card.t -> int -> t -> t

(** converts direction to a string *)
val direction_to_string : direction -> string

(** converts string to a direction *)
val string_to_direction : string -> direction

(**converts color to a string*)
val color_to_string : color -> string

(**converts string to a color*)
val string_to_color : string -> color

(** The number of cards in the deck *)
val deck_length : t -> int

(* Changes state to reflect drawn card *)

(** Given the current state, [draw st] produces the next state
    representing the card drawn into the specified player's hand. *)
val draw : t -> t

(* Changes state to reflect played card *)

(** Given a numbered card from 0-9 and the current state, produces the
    next state with the card removed from the player's hand. Returns an
    InvalidMove exception if the card does not match up with the color*)
val play_numbered_card : Card.t -> t -> t

(** [special_draw card st] produces the next state representing the
    cards drawn into the *next* player's hand. Card.t must be either a
    PlusTwo or a PlusFour card. *)
val special_draw : Card.t -> t -> string -> t

(** [skip card st] skips over the turn of the player after the current
    one. For example, if player 1 played the skip card, then player 3
    would be the next one to go in a 3 player game. *)
val skip : Card.t -> t -> t

(** [choose_color card st] is the state incorporating the color that the
    next player must play. *)
val choose_color : Card.t -> string -> t -> t

(** [special_reverse card dealer_num guess dealer_cards] Reverses the
    direction of play. *)
val reverse : Card.t -> t -> t

(**Use set_ai_length before this method (It relies on it heavily).
   Set_ai_num takes the number of desired ai's and the state of the game
   to find the current number of players (ai or human). It then returns
   a new list of players of the same size but with the desired numbers
   of ai inserted. If the number of AIs exceed the number of players
   then all players will become AIs. The AI_list return is a bool list.
   If false then the player is a human, if true the player is an AI *)
val set_ai_num : int -> t -> t

(**Sets the length of ai_list to the number of players. Use before
   set_ai_num*)
val set_ai_length : int -> t -> t
