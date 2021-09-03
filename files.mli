(** Save takes in a string that is the name of the file to save to. It
    outputs unit*)
val solitaire_save : Solitaire_state.t -> string -> unit

(** Load takes in a string that is the name of the file to load from. It
    outputs a state that was in the file*)
val solitaire_load : string -> Solitaire_state.t

(** Save takes in a string that is the name of the file to save to. It
    outputs unit*)
val uno_save : Uno_state.t -> string -> unit

(** Load takes in a string that is the name of the file to load from. It
    outputs a state that was in the file*)

val uno_load : string -> Uno_state.t
