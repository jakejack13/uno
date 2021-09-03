(** Raised if invlaid move is made with a special action *)
exception InvalidMove

(* GAME INITIALIZATION *)

(** [starting_hands num_players deck] is the array of starting hands of
    seven cards drawn from the deck [deck]. Mutates the deck *)
val starting_hands : int -> Deck.t ref -> Hand.t array

(** [init_discard] is the first card that players must try matching with
    their cards (e.g. if the init_discard is a Blue 5, then the first
    player to go must attempt to play a blue card or a 5 in their turn). *)
val init_discard : Deck.t ref -> Card.t list

(* DRAWING CARDS *)

(** [cards_draw deck num_cards player_cards] is the player's hand
    [player_cards] with the added number of cards [num_cards] drawn from
    the deck [deck]. Mutates the deck. Same as solitaire_game.ml. *)
val cards_draw : Deck.t ref -> int -> Hand.t -> Hand.t

(** [draw deck hand] draws the top card from the deck [deck] and puts it
    in the hand [hand]. Mutates the deck. Same as solitaire_game.ml. *)
val draw : Deck.t ref -> Hand.t -> Hand.t

(* PLAYED CARDS *)

(** [play_numbered_card] is the updated hand after the numbered card has
    been played. Precondition: the card is in the hand. *)
val play_numbered_card : Card.t -> Hand.t -> Hand.t

(** [special_draw card player_cards deck] draws 2 cards from [deck] if
    [card] is a PlusTwo or 4 cards if [card] is a PlusFour and places it
    into the hand [hand]. Same as solitaire_game.ml *)
val special_draw : Card.t -> Hand.t -> Deck.t ref -> Hand.t

(** Skips the turn of the player after the current one. *)
val skip : Card.t -> Hand.t -> Hand.t

(** [special_color card dealer_hand dealer_num] is the color that the
    next player's card *must* match. *)
val color : Card.t -> Hand.t -> Hand.t

(** [special_reverse card dealer_num guess dealer_cards] Reverses the
    direction of play. *)
val reverse : Card.t -> Hand.t -> Hand.t

(** Checks the inputted card to see if the indicator is a numbered card.
    If it is, then true, otherwise returns false*)
val is_numbered_card : Card.t -> bool

(**Easy ai takes a turn for an ai. It takes in the hand of the ai and
   the card on the discard pile. It then compares each card in the hand
   with the discard card to see if it is a valid move. If it is, it
   returns a command.play of the card in the hand that is valid. If no
   cards are valid it returns a command.draw.*)
val easy_ai_play : Hand.t -> Card.t -> Command.command

(**Medium ai takes a turn for an ai. It takes in the hand of the ai and
   the card on the discard pile. It then compares each card in the hand
   with the discard card to see if they have matching indicators. If
   they do then the method returns a command.play of the card in the
   hand that is valid. If no cards match the indicator then it rechecks
   the cards for matching color. If the cards don't match color either
   then the method returns a command.draw.*)
val medium_ai_play : Hand.t -> Card.t -> Command.command

(**Hard ai takes a turn for an ai. It takes in the hand of the ai and
   the card on the discard pile. It then compares each card in the hand
   with the discard card to see if they have matching indicators. If
   they do then the method returns a command.play of the card in the
   hand that is valid. If no cards match the indicator then it rechecks
   the cards for matching color except wild. If the cards don't match
   color either then the cards are rechecked to see if they are the
   color Wild. If none of the cards have a Wild color then the method
   returns a command.draw.*)
val hard_ai_play : Hand.t -> Card.t -> Command.command

(**Easy ai chooses a random color to choose and returns the color as a
   Card.color*)
val easy_ai_choose_color : Card.color

(**Medium ai chooses the first card of the ai's hand (taken in as a card
   list) and choose's that card's color*)
val medium_ai_choose_color : Hand.t -> Card.color

(**Hard ai determines which color the ai has the most of and then
   chooses that color. Example, the AI hand has 5 reds, 1 blue, 3
   greens, and 2 yellows. The AI plays red because it has the most red
   cards*)
val hard_ai_choose_color : Hand.t -> Card.color

(**Given a hand and difficulty, the method runs the corresponding
   choose_color method of the difficulty. If you are using easy
   difficulty, send in an empty hand*)
val ai_choose_color_all : Hand.t -> Command.difficulty -> Card.color

(**Given a hand, card, and difficulty, and the method will run the
   corresponding play method of the difficulty. If you are using easy
   difficulty, send in an empty hand*)
val ai_play_all :
  Hand.t -> Card.t -> Command.difficulty -> Command.command
