type color =
  | Red
  | Blue
  | Green
  | Yellow
  | Wild

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

(** The abstract type of values representing cards is a tuple with its
    first element as its color and its second element as its indicator *)
type t = color * indicator

let create_card c i : t = (c, i)

let color_of_string c =
  if c = "red" || c = "Red" then Red
  else if c = "blue" || c = "Blue" then Blue
  else if c = "green" || c = "Green" then Green
  else if c = "yellow" || c = "Yellow" then Yellow
  else Wild

let indicator_of_string i =
  if i = "one" || i = "One" then One
  else if i = "two" || i = "Two" then Two
  else if i = "three" || i = "Three" then Three
  else if i = "four" || i = "Four" then Four
  else if i = "five" || i = "Five" then Five
  else if i = "six" || i = "Six" then Six
  else if i = "seven" || i = "Seven" then Seven
  else if i = "eight" || i = "Eight" then Eight
  else if i = "nine" || i = "Nine" then Nine
  else if i = "zero" || i = "Zero" then Zero
  else if i = "skip" || i = "Skip" then Skip
  else if i = "reverse" || i = "Reverse" then Reverse
  else if i = "plustwo" || i = "PlusTwo" then PlusTwo
  else if i = "choosecolor" || i = "ChooseColor" then ChooseColor
  else PlusFour

let color_of_card = function c, _ -> c

let indicator_of_card = function _, i -> i

let valid_move top bottom =
  color_of_card top = Wild
  || color_of_card top = color_of_card bottom
  || indicator_of_card top = indicator_of_card bottom

let string_of_color = function
  | Red -> "Red"
  | Blue -> "Blue"
  | Green -> "Green"
  | Yellow -> "Yellow"
  | Wild -> "Wild"

let string_of_indicator = function
  | One -> "One"
  | Two -> "Two"
  | Three -> "Three"
  | Four -> "Four"
  | Five -> "Five"
  | Six -> "Six"
  | Seven -> "Seven"
  | Eight -> "Eight"
  | Nine -> "Nine"
  | Zero -> "Zero"
  | Skip -> "Skip"
  | Reverse -> "Reverse"
  | PlusTwo -> "PlusTwo"
  | ChooseColor -> "ChooseColor"
  | PlusFour -> "PlusFour"

let string_of_card card =
  (card |> color_of_card |> string_of_color)
  ^ " "
  ^ (card |> indicator_of_card |> string_of_indicator)

let int_of_indicator = function
  | One -> 1
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Zero -> 0
  | Skip | Reverse | PlusTwo | ChooseColor | PlusFour -> -1

let to_tuple card = card
