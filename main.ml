let help =
  "Commands:\n\
   new <uno/solitaire>\n\
   play <color> <indicator>\n\
   menu\n\
   draw\n\
   save <filename>\n\
   load <filename>\n\
   quit\n\
   back\n\
   help\n\
   info <uno/solitaire>"

let info_solitaire =
  "In Uno Solitaire, the goal of the player is to match their cards \
   with all four of the dealer’s without exceeding the turn limit of \
   15. \n\
   The player begins the game with 12 cards in their deck. The dealer, \
   on the other hand, begins with 4, all of which are a colored card \
   from 1 to 9. The 4 dealer cards are face down and the player does \
   not know what they are. \n\
   Each turn, the player has the chance to match a numbered card, play \
   a special card, or draw a card into their hand. \n\
   When the player chooses to match, they must choose a numbered card \
   from their deck to play against a card from the dealer. The dealer \
   determines if the color and number of the card played by the player \
   match exactly with the card from the dealer. \n\
   If they do not, the player is given back a number representing the \
   product of the number they played, the dealer’s number, and \
   another integer between 1 and 4 inclusive. If this is the case, \
   their card is returned to them. However, if the cards matched \
   exactly, then both are discarded from the game.\n\
   If the player chooses to draw a card, a random card from the deck \
   is added to their hand. \n\
   If the player chooses to use a special card, then one of the \
   effects below will occur depending on the card played. After a \
   special card is used, it is discarded from the player deck and put \
   back with the pile of unused cards. \n\
   Special cards:\n\
   +2: Draw two cards into your hand. \n\
   +4: Draw four cards into your hand.\n\
   Skip: You can match a card this turn without it factoring into your \
   turn count. \n\
   Choose Color: Find out the color identity of one of the four cards.\n\
   Reverse: You may directly guess the number of one of the dealer’s \
   remaining cards. You will find out if you are correct or incorrect."

let info_uno = ""

let solitaire_draw state =
  match Solitaire_state.draw state with
  | exception Deck.EmptyDeck ->
      print_endline "Deck is empty, cannot draw";
      state
  | s -> s

let rec solitaire_int_input up =
  let text = if up < 5 then "position" else "number" in
  print_endline
    ( "Please input the " ^ text ^ " of the dealer's card (1-"
    ^ string_of_int up ^ ") that you would like to select" );
  print_string "> ";
  match read_line () |> int_of_string_opt with
  | None ->
      print_endline "Please input a valid int";
      solitaire_int_input up
  | Some num ->
      if num < 1 || num > 9 then (
        print_endline ("Please input an int from 1-" ^ string_of_int up);
        solitaire_int_input up )
      else num

let rec solitaire_match_card state card =
  match
    Solitaire_state.match_card card
      (solitaire_int_input (Solitaire_state.dealer_length state))
      state
  with
  | exception Not_found ->
      print_endline "Card not found";
      (state, -11)
  | a -> a

let solitaire_multi_draw state card =
  Solitaire_state.special_draw card state

let rec solitaire_skip state card =
  print_endline "Please play the second card you would like to match";
  print_string "> ";
  match read_line () |> Command.parse with
  | New _ | Menu | Draw | Save _ | Load _ | Quit | Difficulty _ ->
      print_endline "Command not valid at this time, please try again";
      solitaire_skip state card
  | Back -> (state, -1)
  | Play card2 -> (
      match card2 |> Card.indicator_of_card with
      | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
      | Zero -> (
          match
            Solitaire_state.special_skip card card2
              (solitaire_int_input
                 (Solitaire_state.dealer_length state))
              state
          with
          | exception Not_found ->
              print_endline "Card not found";
              solitaire_skip state card
          | a -> a )
      | _ ->
          print_endline
            "Not a valid card to play, must be a number card from 1-9";
          solitaire_skip state card )
  | Help ->
      print_endline help;
      solitaire_skip state card
  | Info game ->
      let text = if game = "uno" then info_uno else info_solitaire in
      print_endline text;
      solitaire_skip state card
  | (exception Command.Empty)
  | (exception Command.Malformed)
  | (exception Command.NotCard) ->
      print_endline "Command is malformed, please try again";
      solitaire_skip state card
  | (exception Command.NotGame) | (exception Command.NotFile) ->
      print_endline
        "Command cannot be performed at this time, please try a valid \
         command";
      solitaire_skip state card

let solitaire_color state card =
  Solitaire_state.special_color card
    (solitaire_int_input (Solitaire_state.dealer_length state))
    state

let solitaire_reverse state card =
  Solitaire_state.special_reverse card
    (solitaire_int_input (Solitaire_state.dealer_length state))
    (solitaire_int_input 9) state

let rec solitaire_play state card =
  match card |> Card.indicator_of_card with
  | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Zero
    ->
      let new_state, num = solitaire_match_card state card in
      if num = -11 then state
      else
        let text =
          if num < 0 then "lower than"
          else if num > 0 then "higher than"
          else "equal to"
        in
        print_endline ("Your card is " ^ text ^ " the dealer's card");
        new_state
  | Skip ->
      let new_state, num = solitaire_skip state card in
      if num = -11 then state
      else
        let text =
          if num < 0 then "lower than"
          else if num > 0 then "higher than"
          else "equal to"
        in
        print_endline ("Your card is " ^ text ^ " the dealer's card");
        new_state
  | Reverse ->
      let new_state, b = solitaire_reverse state card in
      print_endline
        ( if b then "Your guess was correct"
        else "Yoru guess was incorrect" );
      new_state
  | PlusTwo | PlusFour -> solitaire_multi_draw state card
  | ChooseColor ->
      let new_state, new_color = solitaire_color state card in
      print_endline
        ("Color of the card: " ^ Card.string_of_color new_color);
      new_state

let rec solitaire_game state =
  if Solitaire_state.dealer_length state = 0 then
    print_endline "You win!"
  else if Solitaire_state.turns state = 20 then
    print_endline "You ran out of turns. The dealer wins"
  else (
    print_endline "Play a card or Draw";
    print_endline
      ("Turn " ^ (Solitaire_state.turns state |> string_of_int));
    print_endline
      ( "Number of dealer cards left: "
      ^ (Solitaire_state.dealer_length state |> string_of_int) );
    print_endline
      ( "Your cards: "
      ^ (Solitaire_state.current_hand state |> Hand.string_of_hand) );
    print_string "> ";
    match read_line () |> Command.parse with
    | Play card -> solitaire_play state card |> solitaire_game
    | Draw -> solitaire_draw state |> solitaire_game
    | Difficulty d -> failwith "unimplemented"
    | Menu | Quit | Back -> ()
    | Save filename ->
        Files.solitaire_save state filename;
        solitaire_game state
    | Load _ | New _ ->
        print_endline
          "Command cannot be performed at this time, please try a \
           valid command";
        solitaire_game state
    | Help ->
        print_endline help;
        solitaire_game state
    | Info game ->
        let text = if game = "uno" then info_uno else info_solitaire in
        print_endline text;
        solitaire_game state
    | (exception Command.Empty)
    | (exception Command.Malformed)
    | (exception Command.NotCard) ->
        print_endline "Command is malformed, please try again";
        solitaire_game state
    | (exception Command.NotGame) | (exception Command.NotFile) ->
        print_endline
          "Command cannot be performed at this time, please try a \
           valid command";
        solitaire_game state )

let rec uno_select_difficulty () =
  print_endline "Select your difficulty [easy/medium/hard]";
  print_string "> ";
  let input = read_line () in
  if input = "easy" then Command.Easy
  else if input = "medium" then Command.Medium
  else if input = "hard" then Command.Hard
  else (
    print_endline "Incorrect difficulty, please try again";
    uno_select_difficulty () )

let uno_wait_for_cards state =
  "It is Player "
  ^ (Uno_state.current_player state |> string_of_int)
  ^ "'s turn"
  |> print_endline;
  print_endline "Press enter when you want to display your cards";
  let _ = read_line () in
  Uno_state.current_hand (Uno_state.current_player state) state
  |> Hand.string_of_hand |> print_endline

let rec uno_choose_color difficulty state card =
  let choice =
    if
      let lst = Uno_state.current_ai_list state in
      List.nth lst (Uno_state.current_player state - 1)
    then
      Uno_game.ai_choose_color_all
        (Uno_state.current_hand (Uno_state.current_player state) state)
        difficulty
      |> Card.string_of_color
    else (
      print_endline
        "Please input the color you would like to change it to \
         (Red/Blue/Yellow/Green)";
      print_string "> ";
      read_line () )
  in
  if
    choice = "Red" || choice = "Blue" || choice = "Yellow"
    || choice = "Green"
  then Uno_state.choose_color card choice state
  else begin
    print_endline "Incorrect color, please try again";
    uno_choose_color difficulty state card
  end

let rec uno_plus_four difficulty state card =
  let choice =
    if
      let lst = Uno_state.current_ai_list state in
      List.nth lst (Uno_state.current_player state - 1)
    then
      Uno_game.ai_choose_color_all
        (Uno_state.current_hand (Uno_state.current_player state) state)
        difficulty
      |> Card.string_of_color
    else (
      print_endline
        "Please input the color you would like to change it to \
         (Red/Blue/Yellow/Green)";
      print_string "> ";
      read_line () )
  in
  if
    choice = "Red" || choice = "Blue" || choice = "Yellow"
    || choice = "Green"
  then Uno_state.special_draw card state choice
  else begin
    print_endline "Incorrect color, please try again";
    uno_plus_four difficulty state card
  end

let rec uno_play difficulty state card =
  try
    match card |> Card.indicator_of_card with
    | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    | Zero ->
        Uno_state.play_numbered_card card state
    | Skip -> Uno_state.skip card state
    | Reverse -> Uno_state.reverse card state
    | PlusTwo ->
        Uno_state.special_draw card state
          (Card.color_of_card card |> Card.string_of_color)
    | PlusFour -> uno_plus_four difficulty state card
    | ChooseColor -> uno_choose_color difficulty state card
  with Uno_state.WrongCard ->
    print_endline
      "Invalid move, please try again (press enter to continue)";
    let _ = read_line () in
    state

let uno_string_of_top_card state =
  match Uno_state.current_discard state with
  | h :: t ->
      if Card.color_of_card h = Wild then
        Card.string_of_card h ^ " (Color: "
        ^ (Uno_state.current_color state |> Uno_state.color_to_string)
        ^ ")"
      else Card.string_of_card h
  | [] ->
      failwith
        "Invariant broken, must always be a card in the discard pile"

let uno_check_win state =
  if Uno_state.current_hand 1 state |> Hand.hand_size = 0 then 1
  else if Uno_state.current_hand 2 state |> Hand.hand_size = 0 then 2
  else if Uno_state.current_hand 3 state |> Hand.hand_size = 0 then 3
  else if Uno_state.current_hand 4 state |> Hand.hand_size = 0 then 4
  else 0

let uno_ai_move difficulty state =
  let hand =
    Uno_state.current_hand (Uno_state.current_player state) state
  in
  let top =
    match Uno_state.current_discard state with
    | h :: t -> h
    | [] ->
        failwith
          "Invariant broken, must always be a card in the discard pile"
  in
  match Uno_game.ai_play_all hand top difficulty with
  | Play card -> uno_play difficulty state card
  | Draw -> Uno_state.draw state
  | _ -> failwith "impossible operation"

let print_player_num state =
  let rec print_helper num = function
    | h :: t ->
        print_endline
          ( "Player " ^ string_of_int num ^ ": "
          ^ string_of_int (Hand.hand_size h) );
        print_helper (num + 1) t
    | [] -> ()
  in
  print_helper 1 (Uno_state.current_players_card state |> Array.to_list)

let rec uno_game difficulty state =
  if uno_check_win state != 0 then
    print_endline
      ("Player " ^ (uno_check_win state |> string_of_int) ^ " wins!")
  else if
    let lst = Uno_state.current_ai_list state in
    List.nth lst (Uno_state.current_player state - 1)
  then uno_ai_move difficulty state |> uno_game difficulty
  else
    let _ = Sys.command "clear" in
    print_player_num state;
    print_endline "Top card of draw pile: ";
    uno_string_of_top_card state |> print_endline;
    uno_wait_for_cards state;
    print_endline "Play a card or Draw";
    print_string "> ";
    match read_line () |> Command.parse with
    | Play card -> uno_play difficulty state card |> uno_game difficulty
    | Draw -> Uno_state.draw state |> uno_game difficulty
    | Difficulty d -> uno_game d state
    | Menu | Quit | Back -> ()
    | Save filename ->
        Files.uno_save state filename;
        uno_game difficulty state
    | Load _ | New _ ->
        print_endline
          "Command cannot be performed at this time, please try a \
           valid command (press enter to continue)";
        let _ = read_line () in
        uno_game difficulty state
    | Help ->
        print_endline help;
        uno_game difficulty state
    | Info game ->
        let text = if game = "uno" then info_uno else info_solitaire in
        print_endline text;
        uno_game difficulty state
    | (exception Command.Empty)
    | (exception Command.Malformed)
    | (exception Command.NotCard) ->
        print_endline
          "Command is malformed, please try again (press enter to \
           continue)";
        let _ = read_line () in
        uno_game difficulty state
    | (exception Command.NotGame) | (exception Command.NotFile) ->
        print_endline
          "Command cannot be performed at this time, please try a \
           valid command (press enter to continue)";
        let _ = read_line () in
        uno_game difficulty state

let load_game filename =
  let input = filename ^ ".json" in
  let json = Yojson.Basic.from_file input in
  let gametype =
    Yojson.Basic.Util.to_string
      (List.assoc "Game" (Yojson.Basic.Util.to_assoc json))
  in
  if gametype = "Solitaire" then
    let state = Files.solitaire_load filename in
    solitaire_game state
  else
    let state = Files.uno_load filename in
    let difficulty = uno_select_difficulty () in
    uno_game difficulty state

let rec main () =
  print_endline
    "Welcome to Uno command line edition. Please choose [new \
     <uno/solitaire>] for a new game, [load <file>] to load the game \
     from a file, [quit] to quit, or [help] for a list of commands";
  print_string "> ";
  match read_line () |> Command.parse with
  | New name ->
      if name = "uno" then (
        let state =
          Uno_state.init_state 4
          |> Uno_state.set_ai_length 4
          |> Uno_state.set_ai_num 3
        in
        let difficulty = uno_select_difficulty () in
        uno_game difficulty state;
        main () )
      else solitaire_game Solitaire_state.init_state;
      main ()
  | Menu -> main ()
  | Load filename -> load_game filename
  | Back | Quit -> ()
  | Play _ | Draw | Save _ | Difficulty _ | (exception Command.NotCard)
    ->
      print_endline
        "Command cannot be performed at this time, please try a valid \
         command";
      main ()
  | Help ->
      print_endline help;
      main ()
  | Info game ->
      let text = if game = "uno" then info_uno else info_solitaire in
      print_endline text;
      main ()
  | (exception Command.Empty) | (exception Command.Malformed) ->
      print_endline "Command is malformed, please try again";
      main ()
  | exception Command.NotGame ->
      print_endline
        "Not a valid game mode, please select either uno or solitaire";
      main ()
  | exception Command.NotFile ->
      print_endline "File not found, please input a valid file path";
      main ()

let () = main ()
