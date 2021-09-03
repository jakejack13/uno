(** Representation of a hand of cards in the Uno game

    This module represents an instance of a deck of cards in the card
    game and handles all moves done on the hand of cards *)

(** The abstract type of the values representing hands of cards *)
type t

(**Allows for the creation of a hand from a card list*)
val create_hand : Card.t list -> t -> t

(** The value of an empty hand *)
val empty_hand : t

(** Raised when user attempts to remove a card from an empty deck *)
exception EmptyHand

(** Raised when user attempts to remove a card that is not in the deck
    from the deck *)
exception CardNotFound

(** [hand_size hand] is the size of the hand [hand] *)
val hand_size : t -> int

(** [add_card card hand] is the hand [hand] with the card [card] added
    into it *)
val add_card : t -> Card.t -> t

(** [remove_card hand card] is the hand [hand] with the card [card]
    removed from it.

    Raises: [CardNotFound] if the card is not found in the hand

    Raises: [EmptyHand] if the hand is empty *)
val remove_card : t -> Card.t -> t

(** [string_of_hand hand] is the string representation of the hand
    [hand] *)
val string_of_hand : t -> string

(** [list_of_hand hand] is the Card list representation of the hand
    [hand] *)
val list_of_hand : t -> Card.t list

(** [card_in_hand hand card] is true if card [card] is in hand [hand],
    false if otherwise *)
val card_in_hand : t -> Card.t -> bool
