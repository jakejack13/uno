type difficulty =
  | Easy
  | Medium
  | Hard

type command =
  | New of string
  | Play of Card.t
  | Menu
  | Draw
  | Save of string
  | Load of string
  | Quit
  | Back
  | Help
  | Info of string
  | Difficulty of difficulty

(** Raised whenever an empty command is parse. *)
exception Empty

(** Raised whenever Menu or Quit receive extra commands or other
    commands lack extra commands*)
exception Malformed

(** Raised whenever the card given is not a real card*)
exception NotCard

(** Raised whenever game title does not exist*)
exception NotGame

(** Raised whenever file does not exist*)
exception NotFile

let string_to_difficulty st =
  if st = "easy" || st = "Easy" then Easy
  else if st = "medium" || st = "Medium" then Medium
  else Hard

let difficulty_to_string dif =
  match dif with Easy -> "Easy" | Medium -> "Medium" | Hard -> "Hard"

let command_to_string command =
  match command with
  | Menu -> "Menu"
  | Draw -> "Draw"
  | Quit -> "Quit"
  | Back -> "Back"
  | Help -> "Help"
  | New st -> "New" ^ st
  | Save st -> "Save " ^ st
  | Load st -> "Load " ^ st
  | Info st -> "Info " ^ st
  | Play card -> "Play " ^ Card.string_of_card card
  | Difficulty dif -> "Difficulty " ^ difficulty_to_string dif

let parse (str : string) =
  let empty_string = String.trim str in
  if empty_string = "" then raise Empty
  else
    let split_string = String.split_on_char ' ' str in
    match split_string with
    | [] -> raise Empty
    | h :: t ->
        if h = "new" then
          match t with
          | [] -> raise Malformed
          | h :: t ->
              if t = [] then
                if h = "uno" || h = "solitaire" then
                  let d : command = New h in
                  d
                else raise NotGame
              else raise Malformed
        else if h = "play" then
          match t with
          | [] -> raise Malformed
          | h :: t ->
              if h = "red" || h = "blue" || h = "green" || h = "yellow"
              then
                let color = h in
                match t with
                | [] -> raise Malformed
                | h :: t ->
                    if
                      h = "one" || h = "two" || h = "three"
                      || h = "four" || h = "five" || h = "six"
                      || h = "seven" || h = "eight" || h = "nine"
                      || h = "zero" || h = "skip" || h = "reverse"
                      || h = "plustwo"
                    then
                      match t with
                      | [] ->
                          let indicator = h in
                          let card =
                            Card.create_card
                              (Card.color_of_string color)
                              (Card.indicator_of_string indicator)
                          in
                          let d : command = Play card in
                          d
                      | _ -> raise Malformed
                    else raise NotCard
              else if h = "wild" then
                let color = h in
                match t with
                | [] -> raise Malformed
                | h :: t ->
                    if h = "choosecolor" || h = "plusfour" then
                      match t with
                      | [] ->
                          let indicator = h in
                          let card =
                            Card.create_card
                              (Card.color_of_string color)
                              (Card.indicator_of_string indicator)
                          in
                          let d : command = Play card in
                          d
                      | _ -> raise Malformed
                    else raise NotCard
              else raise NotCard
        else if h = "menu" then
          match t with
          | [] ->
              let d : command = Menu in
              d
          | _ -> raise Malformed
        else if h = "save" then
          match t with
          | [] -> raise Malformed
          | h :: t -> (
              match t with
              | [] ->
                  let d : command = Save h in
                  d
              | _ -> raise Malformed )
        else if h = "load" then
          match t with
          | [] -> raise Malformed
          | h :: t -> (
              match t with
              | [] ->
                  if Sys.file_exists (h ^ ".json") then
                    let d : command = Load h in
                    d
                  else raise NotFile
              | _ -> raise Malformed )
        else if h = "quit" then
          match t with
          | [] ->
              let d : command = Quit in
              d
          | _ -> raise Malformed
        else if h = "draw" then
          match t with
          | [] ->
              let d : command = Draw in
              d
          | _ -> raise Malformed
        else if h = "back" then
          match t with
          | [] ->
              let d : command = Back in
              d
          | _ -> raise Malformed
        else if h = "help" then
          match t with
          | [] ->
              let d : command = Help in
              d
          | _ -> raise Malformed
        else if h = "info" then
          match t with
          | [] -> raise Malformed
          | h :: t ->
              if t = [] then
                if h = "uno" || h = "solitaire" then
                  let d : command = Info h in
                  d
                else raise NotGame
              else raise Malformed
        else if h = "difficulty" then
          match t with
          | [] -> raise Malformed
          | h :: t ->
              if t = [] then
                if h = "easy" || h = "medium" || h = "hard" then
                  let d : command =
                    Difficulty (string_to_difficulty h)
                  in
                  d
                else raise NotGame
              else raise Malformed
        else raise Malformed
