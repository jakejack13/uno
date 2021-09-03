(** Test Plan: The test plan for this was to use OUnit testing for every
    single function. Every function has been functionically tested and
    used clear box testing. Some tests were commented out because they
    were so large they would break the formatter. Some tests are
    commented out because the function they test is random so they were
    instead tested indidvually using "make". Our test suite definitely
    demonstrates the correctness of our code because we tested all of
    the edge cases, had full coverage for everything we tested, and it
    also works in main when we play tested for long periods of time.
    Saving was tested by viewing the created .json file to make sure
    everything looked proper. Main was not tested using OUnit since many
    of the functions have side effects. These functions were tested
    using "make play" to play the game normally and check for errors *)

open OUnit2
open Card
open Deck
open Hand
open Command
open Solitaire_game
open Solitaire_state
open Uno_game
open Uno_state

(** [one_to_one x] is [x] *)
let one_to_one x = x

(** [color_of_card_test name card output] is an OUnit test case with
    name [name] that asserts [color_of_card card] is equal to [output] *)
let color_of_card_test name card output =
  name >:: fun _ ->
  assert_equal output (color_of_card card) ~printer:string_of_color

(** [indicator_of_card_test name card output] is an OUnit test case with
    name [name] that asserts [indicator_of_card card] is equal to
    [output] *)
let indicator_of_card_test name card output =
  name >:: fun _ ->
  assert_equal output
    (indicator_of_card card)
    ~printer:string_of_indicator

(** [valid_move_test name top bottom output] is an OUnit test case with
    name [name] that asserts [valid_move top bottom] is equal to
    [output] *)
let valid_move_test name top bottom output =
  name >:: fun _ ->
  assert_equal output (valid_move top bottom) ~printer:Bool.to_string

(** Test cases for Card module *)
let card_tests =
  [
    color_of_card_test "Red color test" (create_card Red One) Red;
    color_of_card_test "Blue color test" (create_card Blue Two) Blue;
    color_of_card_test "Green color test" (create_card Green One) Green;
    color_of_card_test "Yellow color test"
      (create_card Yellow One)
      Yellow;
    color_of_card_test "Wild color test" (create_card Wild One) Wild;
    indicator_of_card_test "One indicator test" (create_card Red One)
      One;
    indicator_of_card_test "Zero indicator test" (create_card Blue Zero)
      Zero;
    indicator_of_card_test "PlusFour indicator test"
      (create_card Yellow PlusFour)
      PlusFour;
    valid_move_test "Valid move color test" (create_card Red Two)
      (create_card Red Seven) true;
    valid_move_test "Valid move indicator test"
      (create_card Green Seven)
      (create_card Yellow Seven)
      true;
    valid_move_test "Valid move wild test"
      (create_card Wild ChooseColor)
      (create_card Red Three) true;
    valid_move_test "Invalid move test"
      (create_card Blue Three)
      (create_card Yellow Two)
      false;
  ]

(** [size_test name deck output] is an OUnit test case with name [name]
    that asserts [size deck] is equal to [output] *)
let deck_size_test name deck output =
  name >:: fun _ ->
  assert_equal output (deck_size deck) ~printer:string_of_int

(** [draw_card_test name deck output] is an OUnit test case with name
    [name] that asserts [draw_card deck] is equal to [output] *)
let draw_card_test name deck output =
  name >:: fun _ ->
  assert_equal output
    (match draw_card deck with a, _ -> a)
    ~printer:string_of_card

(** [draw_card_test name deck output] is an OUnit test case with name
    [name] that asserts [draw_card deck] is equal to [output] *)
let add_card_to_bottom_size_test name deck card output =
  name >:: fun _ ->
  assert_equal output
    (add_card_to_bottom deck card |> deck_size)
    ~printer:string_of_int

(** Test cases for Deck module *)
let deck_tests =
  [
    deck_size_test "Empty deck size test" empty_deck 0;
    deck_size_test "One card deck size test"
      (add_card_to_bottom empty_deck (create_card Red One))
      1;
    deck_size_test "Full deck size test" full_deck 112;
    draw_card_test "Full deck draw card test" full_deck
      (create_card Wild PlusFour);
    draw_card_test "One card deck draw card test"
      (add_card_to_bottom empty_deck (create_card Red One))
      (create_card Red One);
    add_card_to_bottom_size_test "One card add onto empty test"
      empty_deck (create_card Red One) 1;
    add_card_to_bottom_size_test "One card add onto full test" full_deck
      (create_card Red One) 113;
  ]

(** [hand_size_test name deck output] is an OUnit test case with name
    [name] that asserts [hand_size hand] is equal to [output] *)
let hand_size_test name deck output =
  name >:: fun _ ->
  assert_equal output (hand_size deck) ~printer:string_of_int

let hand_remove_exception_test name deck card output =
  name >:: fun _ ->
  let exc =
    match remove_card deck card with
    | exception CardNotFound -> CardNotFound
    | exception EmptyHand -> EmptyHand
    | _ -> assert false
  in
  assert_equal output exc

let one_card_hand = add_card empty_hand (create_card Red One)

(** Test cases for Hand module *)
let hand_tests =
  [
    hand_size_test "Empty hand size test" empty_hand 0;
    hand_size_test "One card hand size test" one_card_hand 1;
    hand_size_test "One card hand remove size test"
      (remove_card one_card_hand (create_card Red One))
      0;
    hand_remove_exception_test "Remove empty hand test" empty_hand
      (create_card Red One) EmptyHand;
    hand_remove_exception_test "Remove card not found hand test"
      one_card_hand (create_card Blue Two) CardNotFound;
  ]

let command_tests_errors name command_string output =
  name >:: fun _ ->
  assert_equal output
    ( match Command.parse command_string with
    | exception Malformed -> "Malformed"
    | exception Empty -> "Empty"
    | exception NotCard -> "NotCard"
    | exception NotGame -> "NotGame"
    | exception NotFile -> "NotFile"
    | _ -> "WRONG" )

let command_test name command_string output =
  name >:: fun _ ->
  assert_equal output
    (Command.parse command_string)
    ~printer:Command.command_to_string

let command_to_string_test name command output =
  name >:: fun _ ->
  assert_equal output (Command.command_to_string command)

let command_tests =
  [
    command_test "quit_test" "quit" Quit;
    command_tests_errors "quit_Malformed_test" "quit extra_word"
      "Malformed";
    command_test "help_test" "help" Help;
    command_tests_errors "help_Malformed_test" "help extra_word"
      "Malformed";
    command_test "back_test" "back" Back;
    command_tests_errors "back_Malformed_test" "back extra_word"
      "Malformed";
    command_test "menu_test" "menu" Menu;
    command_tests_errors "menu_Malformed_test" "menu extra_word"
      "Malformed";
    command_test "quit_test" "draw" Draw;
    command_tests_errors "draw_Malformed_test" "draw extra_word"
      "Malformed";
    command_tests_errors "empty_test" "" "Empty";
    command_tests_errors "empty_test_two" "    " "Empty";
    command_tests_errors "empty_test_two"
      "                               " "Empty";
    command_test "new_test_uno" "new uno" (New "uno");
    command_test "new_test_solitaire" "new solitaire" (New "solitaire");
    command_tests_errors "new_test_solitaire_extra_string"
      "new solitaire bob" "Malformed";
    command_tests_errors "new_test_uno_extra_string" "new uno bob"
      "Malformed";
    command_tests_errors "new_test_no_string" "new" "Malformed";
    command_test "save_test" "save mario" (Save "mario");
    command_tests_errors "save_test_no_string" "save" "Malformed";
    command_tests_errors "save_test_extra_string" "save mario luigi"
      "Malformed";
    command_tests_errors "load_luigi_test" "load luigi" "NotFile";
    command_test "load_valid_test" "load load_test_file"
      (Load "load_test_file");
    command_tests_errors "load_test_no_string" "load" "Malformed";
    command_tests_errors "load_test_extra_string" "save luigi mario"
      "Malformed";
    command_tests_errors "play_test_no_indicator" "play red" "Malformed";
    command_tests_errors "play_test_wrong_color" "play silver five"
      "NotCard";
    command_tests_errors "play_test_wrong_indicator" "play red twenty"
      "NotCard";
    command_tests_errors "play_test_extra_string" "play red one extra"
      "Malformed";
    command_test "correct_color_indicator_red_one" "play red one"
      (Play (Card.create_card Card.Red Card.One));
    command_test "correct_color_indicator_blue_two" "play blue two"
      (Play (Card.create_card Card.Blue Card.Two));
    command_test "correct_color_indicator_green_three"
      "play green three"
      (Play (Card.create_card Card.Green Card.Three));
    command_test "correct_color_indicator_yelloe_four"
      "play yellow four"
      (Play (Card.create_card Card.Yellow Card.Four));
    command_test "correct_color_indicator_wild_plusfour"
      "play wild plusfour"
      (Play (Card.create_card Card.Wild Card.PlusFour));
    command_tests_errors "correct_color_wrong_indicator_red_wrong"
      "play red wrong" "NotCard";
    command_tests_errors "incorrect_color_correct_indicator_wrong_one"
      "play wrong one" "NotCard";
    command_tests_errors "wild_wrong_indicator_test" "play wild one"
      "NotCard";
    command_tests_errors "wild_wrong_indicator_test_two"
      "play wild skip" "NotCard";
    command_test "correct_color_indicator_wild_plusfour"
      "play wild choosecolor"
      (Play (Card.create_card Card.Wild Card.ChooseColor));
    command_test "info_test_uno" "info uno" (Info "uno");
    command_test "info_test_solitaire" "info solitaire"
      (Info "solitaire");
    command_tests_errors "info_test_solitaire_extra_string"
      "info solitaire bob" "Malformed";
    command_to_string_test "menu_test" Command.Menu "Menu";
    command_to_string_test "draw_test" Command.Draw "Draw";
    command_to_string_test "quit_test" Command.Quit "Quit";
    command_to_string_test "back_test" Command.Back "Back";
    command_to_string_test "help_test" Command.Help "Help";
    command_to_string_test "save_test" (Command.Save "file") "Save file";
    command_to_string_test "load_test" (Command.Load "file") "Load file";
    command_to_string_test "info_test" (Command.Info "uno") "Info uno";
    command_to_string_test "play_test"
      (Command.Play (Card.create_card Red Five))
      "Play Red Five";
  ]

let cards_draw_test name deck num_cards player_cards output =
  name >:: fun _ ->
  assert_equal output
    (Solitaire_game.cards_draw deck num_cards player_cards)

let draw_test name deck player_cards output =
  name >:: fun _ ->
  assert_equal output (Solitaire_game.draw deck player_cards)

let is_numbered_card_test name card output =
  name >:: fun _ ->
  assert_equal output (Solitaire_game.is_numbered_card card)

let starting_hand_player_test name deck output =
  name >:: fun _ ->
  assert_equal output (Solitaire_game.starting_hand_player deck)

let special_cards_draw_test name card player_cards deck output =
  name >:: fun _ ->
  assert_equal output
    (Solitaire_game.special_draw card player_cards deck)

let special_cards_draw_test_errors name card player_cards deck output =
  name >:: fun _ ->
  assert_equal output
    ( match Solitaire_game.special_draw card player_cards deck with
    | exception Solitaire_game.InvalidMove -> "InvalidMove"
    | _ -> "WRONG" )

let match_card_test name card dealer_card dealer_hand output =
  name >:: fun _ ->
  assert_equal output
    (Solitaire_game.match_card card dealer_card dealer_hand)
    ~printer:string_of_int

let hand_dealer_test name deck output =
  name >:: fun _ ->
  assert_equal output (Solitaire_game.hand_dealer deck)

let special_color_test name dealer_hand dealer_num output =
  name >:: fun _ ->
  assert_equal output
    (Solitaire_game.special_color
       (Card.create_card
          (Card.color_of_string "wild")
          (Card.indicator_of_string "plusfour"))
       dealer_hand dealer_num)

let special_reverse_test
    name
    (dealer_num : int)
    (guess : int)
    (dealer_cards : Hand.t)
    (output : bool) =
  name >:: fun _ ->
  assert_equal output
    (Solitaire_game.special_reverse
       (Card.create_card
          (Card.color_of_string "wild")
          (Card.indicator_of_string "plusfour"))
       dealer_num guess dealer_cards)

let solitaire_tests =
  [
    cards_draw_test "draw_zero_test" (ref Deck.full_deck) 0
      Hand.empty_hand Hand.empty_hand;
    cards_draw_test "draw_one_test" (ref Deck.full_deck) 1
      Hand.empty_hand
      (Hand.add_card Hand.empty_hand
         (Card.create_card
            (Card.color_of_string "wild")
            (Card.indicator_of_string "plusfour")));
    cards_draw_test "draw_two_test" (ref Deck.full_deck) 2
      Hand.empty_hand
      (Hand.add_card
         (Hand.add_card Hand.empty_hand
            (Card.create_card
               (Card.color_of_string "wild")
               (Card.indicator_of_string "plusfour")))
         (Card.create_card
            (Card.color_of_string "wild")
            (Card.indicator_of_string "choosecolor")));
    draw_test "draw_card_test" (ref Deck.full_deck) Hand.empty_hand
      (Hand.add_card Hand.empty_hand
         (Card.create_card
            (Card.color_of_string "wild")
            (Card.indicator_of_string "plusfour")));
    draw_test "draw_card_non_empty_test" (ref Deck.full_deck)
      (Hand.add_card Hand.empty_hand
         (Card.create_card
            (Card.color_of_string "wild")
            (Card.indicator_of_string "plusfour")))
      (Hand.add_card
         (Hand.add_card Hand.empty_hand
            (Card.create_card
               (Card.color_of_string "wild")
               (Card.indicator_of_string "plusfour")))
         (Card.create_card
            (Card.color_of_string "wild")
            (Card.indicator_of_string "plusfour")));
    is_numbered_card_test "one_test"
      (Card.create_card
         (Card.color_of_string "red")
         (Card.indicator_of_string "one"))
      true;
    is_numbered_card_test "two_test"
      (Card.create_card
         (Card.color_of_string "red")
         (Card.indicator_of_string "two"))
      true;
    is_numbered_card_test "three_test"
      (Card.create_card
         (Card.color_of_string "red")
         (Card.indicator_of_string "three"))
      true;
    is_numbered_card_test "four_test"
      (Card.create_card
         (Card.color_of_string "red")
         (Card.indicator_of_string "four"))
      true;
    is_numbered_card_test "five_test"
      (Card.create_card
         (Card.color_of_string "red")
         (Card.indicator_of_string "five"))
      true;
    is_numbered_card_test "six_test"
      (Card.create_card
         (Card.color_of_string "red")
         (Card.indicator_of_string "six"))
      true;
    is_numbered_card_test "seven_test"
      (Card.create_card
         (Card.color_of_string "red")
         (Card.indicator_of_string "seven"))
      true;
    is_numbered_card_test "eight_test"
      (Card.create_card
         (Card.color_of_string "yellow")
         (Card.indicator_of_string "eight"))
      true;
    is_numbered_card_test "nine_test"
      (Card.create_card
         (Card.color_of_string "green")
         (Card.indicator_of_string "nine"))
      true;
    is_numbered_card_test "zero_test"
      (Card.create_card
         (Card.color_of_string "yellow")
         (Card.indicator_of_string "zero"))
      true;
    is_numbered_card_test "choose_color_test"
      (Card.create_card
         (Card.color_of_string "wild")
         (Card.indicator_of_string "choosecolor"))
      false;
    is_numbered_card_test "plus_four_test"
      (Card.create_card
         (Card.color_of_string "wild")
         (Card.indicator_of_string "plusfour"))
      false;
    is_numbered_card_test "skip_test"
      (Card.create_card
         (Card.color_of_string "green")
         (Card.indicator_of_string "skip"))
      false;
    is_numbered_card_test "reverse_test"
      (Card.create_card
         (Card.color_of_string "blue")
         (Card.indicator_of_string "reverse"))
      false;
    is_numbered_card_test "plustwo_test"
      (Card.create_card
         (Card.color_of_string "red")
         (Card.indicator_of_string "plustwo"))
      false;
    special_cards_draw_test "plusTwo_test"
      (Card.create_card
         (Card.color_of_string "red")
         (Card.indicator_of_string "plustwo"))
      Hand.empty_hand (ref Deck.full_deck)
      (Hand.add_card
         (Hand.add_card Hand.empty_hand
            (Card.create_card
               (Card.color_of_string "wild")
               (Card.indicator_of_string "plusfour")))
         (Card.create_card
            (Card.color_of_string "wild")
            (Card.indicator_of_string "choosecolor")));
    special_cards_draw_test_errors "wrong_test"
      (Card.create_card
         (Card.color_of_string "red")
         (Card.indicator_of_string "one"))
      Hand.empty_hand (ref Deck.full_deck) "InvalidMove";
    special_cards_draw_test "plusfour_test"
      (Card.create_card
         (Card.color_of_string "wild")
         (Card.indicator_of_string "plusfour"))
      Hand.empty_hand (ref Deck.full_deck)
      (Hand.add_card
         (Hand.add_card
            (Hand.add_card
               (Hand.add_card Hand.empty_hand
                  (Card.create_card
                     (Card.color_of_string "wild")
                     (Card.indicator_of_string "plusfour")))
               (Card.create_card
                  (Card.color_of_string "wild")
                  (Card.indicator_of_string "choosecolor")))
            (Card.create_card
               (Card.color_of_string "wild")
               (Card.indicator_of_string "plusfour")))
         (Card.create_card
            (Card.color_of_string "wild")
            (Card.indicator_of_string "choosecolor")));
    (* starting_hand_player_test "default" (ref Deck.full_deck)
       (Hand.add_card (Hand.add_card (Hand.add_card (Hand.add_card
       (Hand.add_card (Hand.add_card (Hand.add_card (Hand.add_card
       (Hand.add_card (Hand.add_card (Hand.add_card (Hand.add_card
       Hand.empty_hand (Card.create_card (Card.color_of_string "wild")
       (Card.indicator_of_string "plusfour"))) (Card.create_card
       (Card.color_of_string "wild") (Card.indicator_of_string
       "choosecolor"))) (Card.create_card (Card.color_of_string "wild")
       (Card.indicator_of_string "plusfour"))) (Card.create_card
       (Card.color_of_string "wild") (Card.indicator_of_string
       "choosecolor"))) (Card.create_card (Card.color_of_string
       "yellow") (Card.indicator_of_string "plustwo")))
       (Card.create_card (Card.color_of_string "yellow")
       (Card.indicator_of_string "reverse"))) (Card.create_card
       (Card.color_of_string "yellow") (Card.indicator_of_string
       "skip"))) (Card.create_card (Card.color_of_string "yellow")
       (Card.indicator_of_string "zero"))) (Card.create_card
       (Card.color_of_string "yellow") (Card.indicator_of_string
       "nine"))) (Card.create_card (Card.color_of_string "yellow")
       (Card.indicator_of_string "eight"))) (Card.create_card
       (Card.color_of_string "yellow") (Card.indicator_of_string
       "seven"))) (Card.create_card (Card.color_of_string "yellow")
       (Card.indicator_of_string "six"))); *)
    match_card_test "matches_test"
      (Card.create_card
         (Card.color_of_string "blue")
         (Card.indicator_of_string "eight"))
      1
      (Hand.add_card Hand.empty_hand
         (Card.create_card
            (Card.color_of_string "blue")
            (Card.indicator_of_string "eight")))
      0;
    (* Commented out because randomized *)
    (* match_card_test "doesnt_matches_test" (Card.create_card
       (Card.color_of_string "blue") (Card.indicator_of_string "eight"))
       9 72; *)
    (* hand_dealer_test "default_deck" (ref Deck.full_deck)
       (Hand.add_card (Hand.add_card (Hand.add_card (Hand.add_card
       Hand.empty_hand (Card.create_card (Card.color_of_string "yellow")
       (Card.indicator_of_string "nine"))) (Card.create_card
       (Card.color_of_string "yellow") (Card.indicator_of_string
       "eight"))) (Card.create_card (Card.color_of_string "yellow")
       (Card.indicator_of_string "seven"))) (Card.create_card
       (Card.color_of_string "yellow") (Card.indicator_of_string
       "six"))); *)
    special_color_test "zero_pos_test"
      (Hand.add_card
         (Hand.add_card
            (Hand.add_card
               (Hand.add_card Hand.empty_hand
                  (Card.create_card
                     (Card.color_of_string "yellow")
                     (Card.indicator_of_string "one")))
               (Card.create_card
                  (Card.color_of_string "blue")
                  (Card.indicator_of_string "two")))
            (Card.create_card
               (Card.color_of_string "green")
               (Card.indicator_of_string "three")))
         (Card.create_card
            (Card.color_of_string "red")
            (Card.indicator_of_string "four")))
      1 Red;
    special_color_test "one_pos_test"
      (Hand.add_card
         (Hand.add_card
            (Hand.add_card
               (Hand.add_card Hand.empty_hand
                  (Card.create_card
                     (Card.color_of_string "yellow")
                     (Card.indicator_of_string "one")))
               (Card.create_card
                  (Card.color_of_string "blue")
                  (Card.indicator_of_string "two")))
            (Card.create_card
               (Card.color_of_string "green")
               (Card.indicator_of_string "three")))
         (Card.create_card
            (Card.color_of_string "red")
            (Card.indicator_of_string "four")))
      2 Green;
    special_color_test "two_pos_test"
      (Hand.add_card
         (Hand.add_card
            (Hand.add_card
               (Hand.add_card Hand.empty_hand
                  (Card.create_card
                     (Card.color_of_string "yellow")
                     (Card.indicator_of_string "one")))
               (Card.create_card
                  (Card.color_of_string "blue")
                  (Card.indicator_of_string "two")))
            (Card.create_card
               (Card.color_of_string "green")
               (Card.indicator_of_string "three")))
         (Card.create_card
            (Card.color_of_string "red")
            (Card.indicator_of_string "four")))
      3 Blue;
    special_color_test "three_pos_test"
      (Hand.add_card
         (Hand.add_card
            (Hand.add_card
               (Hand.add_card Hand.empty_hand
                  (Card.create_card
                     (Card.color_of_string "yellow")
                     (Card.indicator_of_string "one")))
               (Card.create_card
                  (Card.color_of_string "blue")
                  (Card.indicator_of_string "two")))
            (Card.create_card
               (Card.color_of_string "green")
               (Card.indicator_of_string "three")))
         (Card.create_card
            (Card.color_of_string "red")
            (Card.indicator_of_string "four")))
      4 Yellow;
    special_reverse_test "guess_pos_zero_test" 1 4
      (Hand.add_card
         (Hand.add_card
            (Hand.add_card
               (Hand.add_card Hand.empty_hand
                  (Card.create_card
                     (Card.color_of_string "yellow")
                     (Card.indicator_of_string "one")))
               (Card.create_card
                  (Card.color_of_string "blue")
                  (Card.indicator_of_string "two")))
            (Card.create_card
               (Card.color_of_string "green")
               (Card.indicator_of_string "three")))
         (Card.create_card
            (Card.color_of_string "red")
            (Card.indicator_of_string "four")))
      true;
    special_reverse_test "guess_pos_one_test" 2 3
      (Hand.add_card
         (Hand.add_card
            (Hand.add_card
               (Hand.add_card Hand.empty_hand
                  (Card.create_card
                     (Card.color_of_string "yellow")
                     (Card.indicator_of_string "one")))
               (Card.create_card
                  (Card.color_of_string "blue")
                  (Card.indicator_of_string "two")))
            (Card.create_card
               (Card.color_of_string "green")
               (Card.indicator_of_string "three")))
         (Card.create_card
            (Card.color_of_string "red")
            (Card.indicator_of_string "four")))
      true;
    special_reverse_test "guess_pos_two_test" 3 2
      (Hand.add_card
         (Hand.add_card
            (Hand.add_card
               (Hand.add_card Hand.empty_hand
                  (Card.create_card
                     (Card.color_of_string "yellow")
                     (Card.indicator_of_string "one")))
               (Card.create_card
                  (Card.color_of_string "blue")
                  (Card.indicator_of_string "two")))
            (Card.create_card
               (Card.color_of_string "green")
               (Card.indicator_of_string "three")))
         (Card.create_card
            (Card.color_of_string "red")
            (Card.indicator_of_string "four")))
      true;
    special_reverse_test "guess_pos_three_test" 4 1
      (Hand.add_card
         (Hand.add_card
            (Hand.add_card
               (Hand.add_card Hand.empty_hand
                  (Card.create_card
                     (Card.color_of_string "yellow")
                     (Card.indicator_of_string "one")))
               (Card.create_card
                  (Card.color_of_string "blue")
                  (Card.indicator_of_string "two")))
            (Card.create_card
               (Card.color_of_string "green")
               (Card.indicator_of_string "three")))
         (Card.create_card
            (Card.color_of_string "red")
            (Card.indicator_of_string "four")))
      true;
    special_reverse_test "guess_pos_three_test" 1 27
      (Hand.add_card
         (Hand.add_card
            (Hand.add_card
               (Hand.add_card Hand.empty_hand
                  (Card.create_card
                     (Card.color_of_string "yellow")
                     (Card.indicator_of_string "one")))
               (Card.create_card
                  (Card.color_of_string "blue")
                  (Card.indicator_of_string "two")))
            (Card.create_card
               (Card.color_of_string "green")
               (Card.indicator_of_string "three")))
         (Card.create_card
            (Card.color_of_string "red")
            (Card.indicator_of_string "four")))
      false;
  ]

let current_hand_test name hand output =
  name >:: fun _ ->
  assert_equal output
    (Solitaire_state.current_hand hand)
    ~printer:Hand.string_of_hand

let turns_test name st output =
  name >:: fun _ -> assert_equal output (Solitaire_state.turns st)

let deck_length_test name st output =
  name >:: fun _ ->
  assert_equal output
    (Solitaire_state.deck_length st)
    ~printer:string_of_int

let dealer_length_test name st output =
  name >:: fun _ ->
  assert_equal output
    (Solitaire_state.dealer_length st)
    ~printer:string_of_int

let (full_Solitaire_state : Solitaire_state.t) =
  Solitaire_state.create_state (ref Deck.full_deck) Hand.empty_hand
    Hand.empty_hand 5

let (empty_Solitaire_state : Solitaire_state.t) =
  Solitaire_state.create_state (ref Deck.empty_deck) Hand.empty_hand
    Hand.empty_hand 5

let (one_Solitaire_state : Solitaire_state.t) =
  Solitaire_state.create_state
    (ref
       (add_card_to_top empty_deck
          (Card.create_card
             (Card.color_of_string "wild")
             (Card.indicator_of_string "plusfour"))))
    Hand.empty_hand Hand.empty_hand 0

let (two_Solitaire_state : Solitaire_state.t) =
  Solitaire_state.create_state
    (ref
       (add_card_to_bottom
          (add_card_to_top empty_deck
             (Card.create_card
                (Card.color_of_string "wild")
                (Card.indicator_of_string "plusfour")))
          (Card.create_card
             (Card.color_of_string "wild")
             (Card.indicator_of_string "choosecolor"))))
    (Hand.add_card Hand.empty_hand
       (Card.create_card
          (Card.color_of_string "red")
          (Card.indicator_of_string "plustwo")))
    Hand.empty_hand 0

let (four_Solitaire_state : Solitaire_state.t) =
  Solitaire_state.create_state
    (ref
       (add_card_to_bottom
          (add_card_to_bottom
             (add_card_to_bottom
                (add_card_to_top empty_deck
                   (Card.create_card
                      (Card.color_of_string "wild")
                      (Card.indicator_of_string "plusfour")))
                (Card.create_card
                   (Card.color_of_string "wild")
                   (Card.indicator_of_string "choosecolor")))
             (Card.create_card
                (Card.color_of_string "yellow")
                (Card.indicator_of_string "one")))
          (Card.create_card
             (Card.color_of_string "blue")
             (Card.indicator_of_string "two"))))
    (Hand.add_card Hand.empty_hand
       (Card.create_card
          (Card.color_of_string "red")
          (Card.indicator_of_string "plusfour")))
    Hand.empty_hand 0

let draw_test name st output =
  name >:: fun _ -> assert_equal output (Solitaire_state.draw st)

let special_draw_two_test name st output =
  name >:: fun _ ->
  assert_equal output
    (Solitaire_state.special_draw
       (Card.create_card
          (Card.color_of_string "red")
          (Card.indicator_of_string "plustwo"))
       st)

let special_draw_four_test name st output =
  name >:: fun _ ->
  assert_equal output
    (Solitaire_state.special_draw
       (Card.create_card
          (Card.color_of_string "red")
          (Card.indicator_of_string "plusfour"))
       st)

let state_tests =
  [
    (* (let (x : Solitaire_state.t) =
       Solitaire_state.init_Solitaire_state in current_hand_test
       "initial_hand" x Hand.empty_hand); *)
    turns_test "zero_turns_test" Solitaire_state.init_state 0;
    turns_test "five_turns_test" full_Solitaire_state 5;
    deck_length_test "full_deck_test" Solitaire_state.init_state 96;
    deck_length_test "empty_deck_test" empty_Solitaire_state 0;
    dealer_length_test "init_dealer_test" Solitaire_state.init_state 4;
    dealer_length_test "empty_dealer_test" empty_Solitaire_state 0;
    draw_test "drawing_empty_test" one_Solitaire_state
      (Solitaire_state.create_state
         (Solitaire_state.get_deck one_Solitaire_state)
         (Hand.add_card Hand.empty_hand
            (Card.create_card
               (Card.color_of_string "wild")
               (Card.indicator_of_string "plusfour")))
         Hand.empty_hand 1);
    special_draw_two_test "drawing_two_test" two_Solitaire_state
      (Solitaire_state.create_state
         (Solitaire_state.get_deck two_Solitaire_state)
         (Hand.add_card
            (Hand.add_card Hand.empty_hand
               (Card.create_card
                  (Card.color_of_string "wild")
                  (Card.indicator_of_string "plusfour")))
            (Card.create_card
               (Card.color_of_string "wild")
               (Card.indicator_of_string "choosecolor")))
         Hand.empty_hand 1);
    special_draw_four_test "drawing_four_test" four_Solitaire_state
      (Solitaire_state.create_state
         (Solitaire_state.get_deck four_Solitaire_state)
         (Hand.add_card
            (Hand.add_card
               (Hand.add_card
                  (Hand.add_card Hand.empty_hand
                     (Card.create_card
                        (Card.color_of_string "wild")
                        (Card.indicator_of_string "plusfour")))
                  (Card.create_card
                     (Card.color_of_string "wild")
                     (Card.indicator_of_string "choosecolor")))
               (Card.create_card
                  (Card.color_of_string "yellow")
                  (Card.indicator_of_string "one")))
            (Card.create_card
               (Card.color_of_string "blue")
               (Card.indicator_of_string "two")))
         Hand.empty_hand 1);
  ]

let solitaire_load_test name file_name output =
  name >:: fun _ -> assert_equal output (Files.solitaire_load file_name)

let uno_load_test name file_name output =
  name >:: fun _ -> assert_equal output (Files.uno_load file_name)

let file_tests =
  [
    (let solitaire_save_test =
       Files.solitaire_save
         (Solitaire_state.create_state
            (ref
               (Deck.add_card_to_bottom Deck.empty_deck
                  (Card.create_card
                     (Card.color_of_string "red")
                     (Card.indicator_of_string "five"))))
            (Hand.add_card Hand.empty_hand
               (Card.create_card
                  (Card.color_of_string "green")
                  (Card.indicator_of_string "three")))
            (Hand.add_card Hand.empty_hand
               (Card.create_card
                  (Card.color_of_string "blue")
                  (Card.indicator_of_string "two")))
            6)
         "solitaire_save_test"
     in
     solitaire_save_test;
     solitaire_load_test "load_basic_solitaire_test"
       "solitaire_save_test"
       (Solitaire_state.create_state
          (ref
             (Deck.add_card_to_bottom Deck.empty_deck
                (Card.create_card
                   (Card.color_of_string "red")
                   (Card.indicator_of_string "five"))))
          (Hand.add_card Hand.empty_hand
             (Card.create_card
                (Card.color_of_string "green")
                (Card.indicator_of_string "three")))
          (Hand.add_card Hand.empty_hand
             (Card.create_card
                (Card.color_of_string "blue")
                (Card.indicator_of_string "two")))
          6));
    (let uno_save_test =
       Files.uno_save
         (Uno_state.create_state
            (ref
               (Deck.add_card_to_bottom Deck.empty_deck
                  (Card.create_card
                     (Card.color_of_string "red")
                     (Card.indicator_of_string "six"))))
            [|
              Hand.add_card Hand.empty_hand
                (Card.create_card
                   (Card.color_of_string "blue")
                   (Card.indicator_of_string "seven"));
              Hand.add_card Hand.empty_hand
                (Card.create_card
                   (Card.color_of_string "green")
                   (Card.indicator_of_string "three"));
              Hand.add_card Hand.empty_hand
                (Card.create_card
                   (Card.color_of_string "red")
                   (Card.indicator_of_string "five"));
              Hand.add_card Hand.empty_hand
                (Card.create_card
                   (Card.color_of_string "wild")
                   (Card.indicator_of_string "choosecolor"));
            |]
            [
              Card.create_card
                (Card.color_of_string "green")
                (Card.indicator_of_string "skip");
            ]
            5
            (Uno_state.string_to_direction "forward")
            (Uno_state.string_to_color "red")
            [ true; true; false; false; true ])
         "uno_save_test"
     in

     uno_save_test;
     uno_load_test "load_basic_uno_test" "uno_save_test"
       (Uno_state.create_state
          (ref
             (Deck.add_card_to_bottom Deck.empty_deck
                (Card.create_card
                   (Card.color_of_string "red")
                   (Card.indicator_of_string "six"))))
          [|
            Hand.add_card Hand.empty_hand
              (Card.create_card
                 (Card.color_of_string "blue")
                 (Card.indicator_of_string "seven"));
            Hand.add_card Hand.empty_hand
              (Card.create_card
                 (Card.color_of_string "green")
                 (Card.indicator_of_string "three"));
            Hand.add_card Hand.empty_hand
              (Card.create_card
                 (Card.color_of_string "red")
                 (Card.indicator_of_string "five"));
            Hand.add_card Hand.empty_hand
              (Card.create_card
                 (Card.color_of_string "wild")
                 (Card.indicator_of_string "choosecolor"));
          |]
          [
            Card.create_card
              (Card.color_of_string "green")
              (Card.indicator_of_string "skip");
          ]
          5
          (Uno_state.string_to_direction "forward")
          (Uno_state.string_to_color "red")
          [ true; true; false; false; true ]));
  ]

(** Test suite for uno *)

let hand_length_test name st player_num output =
  name >:: fun _ ->
  assert_equal output
    (Hand.hand_size
       (Uno_state.current_players_card st).(player_num - 1))
    ~printer:string_of_int

let color_test name st output =
  name >:: fun _ ->
  assert_equal output (color_to_string (current_color st))

let current_turn_test name st output =
  name >:: fun _ ->
  assert_equal output
    (Uno_state.current_player st)
    ~printer:string_of_int

let st4 = Uno_state.set_player_num 4 (Uno_state.init_state 4)

let st4_2 = Uno_state.set_player_num 4 (Uno_state.init_state 4)

let st_reverse = Uno_state.set_player_num 4 (Uno_state.init_state 4)

let plusfour =
  Card.create_card
    (Card.color_of_string "wild")
    (Card.indicator_of_string "plusfour")

let plustwo =
  Card.create_card
    (Card.color_of_string "wild")
    (Card.indicator_of_string "plustwo")

let reverse =
  Card.create_card
    (Card.color_of_string "wild")
    (*only making color wild for purpose of testing*)
    (Card.indicator_of_string "reverse")

let wildfouradded = Uno_state.add_to_player_hand plusfour 4 st4

let st4_special_four =
  Uno_state.special_draw plusfour wildfouradded "Red"

let wildtwoadded = Uno_state.add_to_player_hand plustwo 4 st4_2

let st4_special_two = Uno_state.special_draw plustwo wildtwoadded "Blue"

let reverse_added = Uno_state.add_to_player_hand reverse 4 st_reverse

let st4_reverse = Uno_state.reverse reverse reverse_added

let ai_play_all_test name hand card difficulty output =
  name >:: fun _ ->
  assert_equal output (Uno_game.ai_play_all hand card difficulty)

let easy_ai_play_test name hand card output =
  name >:: fun _ ->
  assert_equal output
    (Uno_game.easy_ai_play hand card)
    ~printer:Command.command_to_string

let medium_ai_play_test name hand card output =
  name >:: fun _ ->
  assert_equal output
    (Uno_game.medium_ai_play hand card)
    ~printer:Command.command_to_string

let hard_ai_play_test name hand card output =
  name >:: fun _ ->
  assert_equal output
    (Uno_game.hard_ai_play hand card)
    ~printer:Command.command_to_string

let ai_choose_color_all_test name hand difficulty output =
  name >:: fun _ ->
  assert_equal output (Uno_game.ai_choose_color_all hand difficulty)

let easy_ai_choose_color_test name output =
  name >:: fun _ -> assert_equal output Uno_game.easy_ai_choose_color

let medium_ai_choose_color_test name hand output =
  name >:: fun _ ->
  assert_equal output (Uno_game.medium_ai_choose_color hand)

let hard_ai_choose_color_test name hand output =
  name >:: fun _ ->
  assert_equal output (Uno_game.hard_ai_choose_color hand)

let wild_testing_hand =
  ( Hand.add_card Hand.empty_hand (Card.create_card Card.Blue Card.Eight)
  |> Hand.add_card )
    (Card.create_card Card.Wild Card.PlusFour)

let wildless_testing_hand =
  Hand.add_card Hand.empty_hand (Card.create_card Card.Blue Card.Eight)

let priority_testing_hand =
  ( ( Hand.add_card Hand.empty_hand
        (Card.create_card Card.Green Card.Two)
    |> Hand.add_card )
      (Card.create_card Card.Green Card.Skip)
  |> Hand.add_card )
    (Card.create_card Card.Wild Card.PlusFour)

let priority_two_testing_hand =
  ( ( Hand.add_card Hand.empty_hand
        (Card.create_card Card.Blue Card.Three)
    |> Hand.add_card )
      (Card.create_card Card.Blue Card.Skip)
  |> Hand.add_card )
    (Card.create_card Card.Wild Card.PlusFour)

let hard_choose_color_testing_hand =
  ( ( ( ( ( ( ( ( ( Hand.add_card Hand.empty_hand
                      (Card.create_card Card.Red Card.Three)
                  |> Hand.add_card )
                    (Card.create_card Card.Red Card.Skip)
                |> Hand.add_card )
                  (Card.create_card Card.Blue Card.One)
              |> Hand.add_card )
                (Card.create_card Card.Blue Card.Skip)
            |> Hand.add_card )
              (Card.create_card Card.Blue Card.Nine)
          |> Hand.add_card )
            (Card.create_card Card.Blue Card.Three)
        |> Hand.add_card )
          (Card.create_card Card.Yellow Card.Two)
      |> Hand.add_card )
        (Card.create_card Card.Red Card.Three)
    |> Hand.add_card )
      (Card.create_card Card.Green Card.Seven)
  |> Hand.add_card )
    (Card.create_card Card.Green Card.Eight)

let hard_choose_color_two_testing_hand =
  ( ( ( ( ( ( ( ( ( Hand.add_card Hand.empty_hand
                      (Card.create_card Card.Green Card.Three)
                  |> Hand.add_card )
                    (Card.create_card Card.Wild Card.PlusFour)
                |> Hand.add_card )
                  (Card.create_card Card.Wild Card.PlusFour)
              |> Hand.add_card )
                (Card.create_card Card.Red Card.Skip)
            |> Hand.add_card )
              (Card.create_card Card.Red Card.Nine)
          |> Hand.add_card )
            (Card.create_card Card.Red Card.Three)
        |> Hand.add_card )
          (Card.create_card Card.Wild Card.PlusFour)
      |> Hand.add_card )
        (Card.create_card Card.Wild Card.PlusFour)
    |> Hand.add_card )
      (Card.create_card Card.Wild Card.PlusFour)
  |> Hand.add_card )
    (Card.create_card Card.Blue Card.Eight)

let uno_state_tests =
  [
    hand_length_test "P1 length after P4 plays plus4 should be 11"
      st4_special_four 1 11;
    hand_length_test "P2 length should be 7" st4_special_four 2 7;
    hand_length_test "P3 length should be 7" st4_special_four 3 7;
    hand_length_test "P4 length should be 6" st4_special_four 4 7;
    color_test "New color should be red" st4_special_four "Red";
    hand_length_test "P1 length after P4 plays plus2 should be 9"
      st4_special_two 1 9;
    hand_length_test "P2 length after P4 plays plus2 should be 7"
      st4_special_two 2 7;
    hand_length_test "P3 length after P4 plays plus2 should be 7"
      st4_special_two 3 7;
    hand_length_test "P4 length after P4 plays plus2 should be 6"
      st4_special_two 4 7;
    color_test "New color should be blue" st4_special_two "None";
    current_turn_test "P3 next after P4 plays Rev" st4_reverse 3;
    easy_ai_play_test "play_wild_easy_test" wild_testing_hand
      (Card.create_card Card.Blue Card.Eight)
      (Command.Play (Card.create_card Card.Wild Card.PlusFour));
    medium_ai_play_test "play_wild_medium_test" wild_testing_hand
      (Card.create_card Card.Blue Card.Eight)
      (Command.Play (Card.create_card Card.Blue Card.Eight));
    hard_ai_play_test "play_wild_hard_test" wild_testing_hand
      (Card.create_card Card.Blue Card.Eight)
      (Command.Play (Card.create_card Card.Blue Card.Eight));
    easy_ai_play_test "play_wildless_easy_test" wildless_testing_hand
      (Card.create_card Card.Green Card.Seven)
      Command.Draw;
    medium_ai_play_test "play_wildless_medium_test"
      wildless_testing_hand
      (Card.create_card Card.Green Card.Seven)
      Command.Draw;
    hard_ai_play_test "play_wildless_hard_test" wildless_testing_hand
      (Card.create_card Card.Green Card.Seven)
      Command.Draw;
    easy_ai_play_test "play_priority_easy_test" priority_testing_hand
      (Card.create_card Card.Green Card.Two)
      (Command.Play (Card.create_card Card.Wild Card.PlusFour));
    medium_ai_play_test "play_priority_medium_test"
      priority_testing_hand
      (Card.create_card Card.Green Card.Two)
      (Command.Play (Card.create_card Card.Green Card.Two));
    hard_ai_play_test "play_priority_hard_test" priority_testing_hand
      (Card.create_card Card.Green Card.Two)
      (Command.Play (Card.create_card Card.Green Card.Two));
    easy_ai_play_test "play_priority_two_easy_test"
      priority_two_testing_hand
      (Card.create_card Card.Blue Card.Two)
      (Command.Play (Card.create_card Card.Wild Card.PlusFour));
    medium_ai_play_test "play_priority_two_medium_test"
      priority_two_testing_hand
      (Card.create_card Card.Blue Card.Two)
      (Command.Play (Card.create_card Card.Wild Card.PlusFour));
    hard_ai_play_test "play_priority_two_hard_test"
      priority_two_testing_hand
      (Card.create_card Card.Blue Card.Two)
      (Command.Play (Card.create_card Card.Blue Card.Skip));
    medium_ai_choose_color_test "choose_color_priority_medium_test"
      priority_testing_hand Card.Green;
    medium_ai_choose_color_test "choose_color_priority_two_medium_test"
      priority_two_testing_hand Card.Blue;
    hard_ai_choose_color_test "choose_color_hard_test"
      hard_choose_color_testing_hand Card.Blue;
    hard_ai_choose_color_test "choose_color_two_hard_test"
      hard_choose_color_two_testing_hand Card.Red;
    ai_choose_color_all_test "choose_color_two_all_test"
      hard_choose_color_two_testing_hand Hard Card.Red;
    ai_choose_color_all_test "choose_color_priority_medium_test"
      priority_testing_hand Medium Card.Green;
    ai_play_all_test "play_priority_two_hard_test"
      priority_two_testing_hand
      (Card.create_card Card.Blue Card.Two)
      Hard
      (Command.Play (Card.create_card Card.Blue Card.Skip));
    ai_play_all_test "play_priority_two_medium_test"
      priority_two_testing_hand
      (Card.create_card Card.Blue Card.Two)
      Medium
      (Command.Play (Card.create_card Card.Wild Card.PlusFour));
    ai_play_all_test "play_priority_two_easy_test"
      priority_two_testing_hand
      (Card.create_card Card.Blue Card.Two)
      Easy
      (Command.Play (Card.create_card Card.Wild Card.PlusFour));
  ]

let suite =
  "test suite for uno"
  >::: List.flatten
         [
           card_tests;
           deck_tests;
           command_tests;
           solitaire_tests;
           state_tests;
           file_tests;
           uno_state_tests;
         ]

let _ = run_test_tt_main suite
