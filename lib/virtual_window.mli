(** Virtual_window provides functions to create virtual window to improve rendering speed. But Virtual_module provide
    only row-base solution. Do not provide column-base solution because calculation for column is not independent some
    of context to draw. *)

type t
(** Abstract type *)

(** Calculated window contains row size to draw effective area, start and end index of total rows. *)
module Window : sig
  type t = private {
    size : int;
    start_index : int;
    end_index : int;
    focused_index : int;
  }

  (** !Accessors *)

  val size : t -> int

  val start_index : t -> int

  val end_index : t -> int

  val focused_index : t -> int

  val pp : Format.formatter -> t -> unit
  (** pretty print for [t]window *)

  val show : t -> string
  (** get string representation of [t]window *)

  val equal : t -> t -> bool
  (** [equal v1 v2] return same or not between values *)
end

val create : unit -> t
(** [create ()] create new [t]type. *)

val update_total_rows : int -> t -> t
(** [update_total_rows new_rows t] update total rows of [t] virtual window. *)

val update_focused_row : int -> t -> t
(** [update_focused_row focused_row t] update focused row that is 0 origin. This function effects start and end index of
    window. *)

val update_view_port_size : int -> t -> t
(** [update_view_size size t] update view port size to calculate window *)

val calculate_window : t -> Window.t
(** [calculate_window t] return a calculated window from total rows and focused row. *)
