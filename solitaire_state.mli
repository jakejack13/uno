(** The abstract type of values representing the game state *)
type t

(** [get_deck] returns the deck ref of the inputted state*)
val get_deck : t -> Deck.t ref

(** [create state] allows testing of state methods by sending in inputs
    to make a state with specified values*)
val create_state : Deck.t ref -> Hand.t -> Hand.t -> int -> t

(** [init state] The initial state of the game when playing uno
    solitaire. In this state, the deck is full apart from the cards that
    have been distributed to the player and dealer, and the turn count
    is 0. *)
val init_state : t

(** The list of cards that the player currently has *)
val current_hand : t -> Hand.t

(** The list of cards that the dealer currently has *)
val dealer_hand : t -> Hand.t

(** [turns st] is the number of turns taken for the given state [st] *)
val turns : t -> int

(** The number of cards in the deck *)
val deck_length : t -> int

(** The number of cards the dealer has left *)
val dealer_length : t -> int

(** [match_card st] produces the next state if the card is valid and one
    of [One, ..., Nine]. Precondition: the command typed into the
    command line is match and the card is valid. *)
val match_card : Card.t -> int -> t -> t * int

(** [draw st] produces the next state representing the card drawn if the
    turn limit has not been exceeded *)
val draw : t -> t

(** [special_draw card st] produces the next state representing the
    cards drawn if the turn limit has not been exceeded *)
val special_draw : Card.t -> t -> t

(** [special_skip card1 card2 dealer_card st] produces the next state if
    the first card is a skip and the second card is a valid card from
    1-9. *)
val special_skip : Card.t -> Card.t -> int -> t -> t * int

(** [special_color card dealer_card st] produces the color of the
    dealer_card chosen provided the card used is a color card. *)
val special_color : Card.t -> int -> t -> t * Card.color

(** [special_reverse card dealer_num guess st] returns true if [guess]
    is equal to the int representation of the [dealer_num]th card in the
    state [st], false if otherwise Example: 1 5, returns if 5 is the
    number on the dealer's first card *)
val special_reverse : Card.t -> int -> int -> t -> t * bool
