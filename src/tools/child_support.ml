(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

(** Utility for operators that need to control child source clocks. *)

open Clock.Tick

class virtual base ~check_self_sync children_val =
  let children = List.map Lang.to_source children_val in
  object (self)
    initializer
    if check_self_sync then
      List.iter
        (fun c ->
          if (Lang.to_source c)#self_sync <> (`Static, false) then
            raise
              (Error.Invalid_value
                 ( c,
                   "This source may control its own latency and cannot be used \
                    with this operator." )))
        children_val

    val mutable child_clock = None
    val mutable needs_tick = true
    method virtual id : string
    method virtual clock : Clock.t
    method virtual on_initialize : (unit -> unit) -> unit
    method virtual on_before_streaming_cycle_start : (unit -> unit) -> unit
    method virtual on_after_get_frame : (unit -> unit Clock.tick) -> unit
    method private child_clock = Option.get child_clock

    initializer
    self#on_initialize (fun () ->
        let c =
          Clock.make ~sync:`None ~id:(Printf.sprintf "%s.child" self#id) ()
        in
        child_clock <- Some c;
        List.iter (fun s -> s#set_clock c) children);
    self#on_before_streaming_cycle_start (fun () -> needs_tick <- true);
    self#on_after_get_frame (fun () ->
        if needs_tick then self#child_clock#tick >> fun _ -> return ()
        else return ())
  end
