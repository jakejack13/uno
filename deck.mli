(** Representation of a deck of cards in the Uno game

    This module represents an instance of a deck of cards in the card
    game and handles all moves done on the deck of cards *)

(** The abstract type of values representing decks of cards *)
type t

(** The value of an empty deck *)
val empty_deck : t

(** The deck containing all of the cards present in a game ordered by
    how they appear in the Card module *)
val full_deck : t

(** Raised when user attempts to draw from an empty deck *)
exception EmptyDeck

(**Allows thec reation of a deck from a Card list*)
val create_deck : Card.t list -> t -> t

(** [size deck] is the number of cards in the deck [deck] *)
val deck_size : t -> int

(** [draw_card deck] is the tuple of the card drawn from the top of the
    deck [deck] and the rest of the deck. Raises EmptyDeck if deck is
    empty *)
val draw_card : t -> Card.t * t

(** [peek_card deck] is the card at the top of the deck [deck] *)
val peek_card : t -> Card.t

(** [add_card_to_bottom deck card] adds the card [card] to the bottom of
    the deck [deck] *)
val add_card_to_bottom : t -> Card.t -> t

(** [add_card_to_top deck card] adds the card [card] to the bottom of
    the deck [deck] *)
val add_card_to_top : t -> Card.t -> t

(** [shuffle_deck deck] randomizes the order of the cards in the deck
    [deck] *)
val shuffle_deck : t -> t

(** [to_list deck] is the list of Card.t representation of the deck
    [deck] *)
val to_list : t -> Card.t list
