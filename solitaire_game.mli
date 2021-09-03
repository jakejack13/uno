(** Raised if invlaid move is made with a special action *)
exception InvalidMove

(** [cards_draw deck num_cards player_cards] is the player's hand
    [player_cards] with the added number of cards [num_cards] drawn from
    the deck [deck]. Mutates the deck *)
val cards_draw : Deck.t ref -> int -> Hand.t -> Hand.t

(** [starting_hand_player deck] is the starting hand of twelve cards
    drawn from the deck [deck]. Mutates the deck *)
val starting_hand_player : Deck.t ref -> Hand.t

(** [hand_dealer deck] is the starting hand of four cards drawn from the
    deck [deck]. Mutates the deck. These cards may only be numbered
    cards from 1-9. *)
val hand_dealer : Deck.t ref -> Hand.t

(** [match_card card dealer_card] is 0 if the int representation of
    [card] and [dealer_card] are equal, otherwise it is [card] -
    [dealer_card] *)
val match_card : Card.t -> int -> Hand.t -> int

(** [draw deck hand] draws the top card from the deck [deck] and puts it
    in the hand [hand]. Mutates the deck. *)
val draw : Deck.t ref -> Hand.t -> Hand.t

(** [special_draw card player_cards deck] draws 2 cards from [deck] if
    [card] is a PlusTwo or 4 cards if [card] is a PlusFour and places it
    into the hand [hand]*)
val special_draw : Card.t -> Hand.t -> Deck.t ref -> Hand.t

(** [special_skip card1 card2 dealer_num] is
    [match_card card2 dealer_num] *)
val special_skip : Card.t -> Card.t -> int -> Hand.t -> int

(* We need to remove "card" as an input here *)

(** [special_color card dealer_hand dealer_num] is the color of the
    [dealer_num]th card in [dealer_hand] Example: Player uses Choose
    Color and picks 1, finds out the color of card 1 from the dealer. *)
val special_color : Card.t -> Hand.t -> int -> Card.color

(* We need to remove "card" as an input here *)

(** [special_reverse card dealer_num guess dealer_cards] returns true if
    [guess] is equal to the int representation of the [dealer_num]th
    card in [dealer_cards], false if otherwise Example: 0 5, returns
    true if 5 is the number on the dealer's first card *)
val special_reverse : Card.t -> int -> int -> Hand.t -> bool

(** Checks the inputted card to see if the indicator is a numbered card.
    If it is, then true, otherwise returns false*)
val is_numbered_card : Card.t -> bool
