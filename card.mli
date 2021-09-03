(** Representation of a card in the Uno game

    This module represents an instance of a card in the card game and
    handles all comparisons and moves between these cards. *)

(** The abstract type of values representing cards *)
type t

(** The type of the colors of the cards *)
type color =
  | Red
  | Blue
  | Green
  | Yellow
  | Wild

(** The type of the indicator of the cards *)
type indicator =
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Zero
  | Skip
  | Reverse
  | PlusTwo
  | ChooseColor
  | PlusFour

(** [create_card c i] is the card with the color [c] and the indicator
    [i] *)
val create_card : color -> indicator -> t

(** [color_of_string c] is the color that the string [c] represents.
    Requires: c is a valid color *)
val color_of_string : string -> color

(** [indicator_of_string i] is the indicator that the string [i]
    represets. Requires: i is a valid indicator *)
val indicator_of_string : string -> indicator

(** [color_of_card card] is the color of the card [card] *)
val color_of_card : t -> color

(** [indicator_of_card card] is the indicator of the card [card] *)
val indicator_of_card : t -> indicator

(** [valid_move top bottom] is if placing the top card [top] on the
    bottom card [bottom] is a valid move in the game of Uno *)
val valid_move : t -> t -> bool

(** [string_of_color color] is the string representation of the color
    [color] *)
val string_of_color : color -> string

(** [string_of_indicator indicator] is the string representation of the
    indicator [indicator] *)
val string_of_indicator : indicator -> string

(** [string_of_card card] is the string representation of the card
    [card] *)
val string_of_card : t -> string

(** [int_of_indicator indicator] is the int representation of the
    indicator [indicator] or -1 if the indicator cannot be converted to
    an int *)
val int_of_indicator : indicator -> int

val to_tuple : t -> color * indicator
