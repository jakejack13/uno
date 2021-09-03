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

(** Raised whenever Menu or Quit receive extra commands*)
exception Malformed

(** Raised whenever the card given is not a real card*)
exception NotCard

(** Raised whenever game title does not exist*)
exception NotGame

(** Raised whenever file does not exist*)
exception NotFile

(** [parse str] parses a player's input into a [command], as follows.
    The first word (i.e., consecutive sequence of non-space characters)
    of [str] becomes the command. The rest of the words, if any, become
    the name of the file, game, or Card.t. Examples:

    - [parse "new uno"] is [New \"Uno"\] - [parse "play red one"] is
    [Play \{Red; One}\] - [parse "menu"] is [Menu] - [parse "save
    savefile.json"] is [Save \"savefile"\] - [parse "load
    loadfile.json"] is [Load \"loadfile"\] - [parse "quit"] is [Quit]-
    [parse "draw"] is [Draw] - [parse "help"] is [Help] - [parse "back"]
    is [Back] [parse "info uno"] is [Info \"Uno"\]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [str] is the empty string or contains only
    spaces.

    Raises: [NotCard] if the specified card is not a real color or
    indicator

    Raises: [NotGame] if the specified game does not exist

    Raises: [NotFile] if the specified string when loading does not
    exist.

    Raises: [Malformed] if the command is malformed. A command is
    malformed if the Command give is missing info or has extra info *)
val parse : string -> command

(**Converts the command into a string for testing*)
val command_to_string : command -> string

val difficulty_to_string : difficulty -> string

val string_to_difficulty : string -> difficulty
