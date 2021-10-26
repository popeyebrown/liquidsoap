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

(* [`Start] and [`Stop] are explicit states resulting of a
   user command. [`Idle] is a default state at init or after a source
   failure. [`Idle] is used to captures situations where the output is
   stopped by can be restarted immediately when its underlying source
   becomes available again. In contrast, [`Stopped] indicates an
   explicit user-requested stop and the output will not automatically
   restart. *)
type state = [ `Started | `Stopped | `Idle ]

(** Class for sources with start/stop methods. Class inheriting it should
    declare their own [start]/[stop] method. *)
class virtual base ~(on_start : unit -> unit) ~(on_stop : unit -> unit)
  ~autostart () =
  object (self)
    (* Source methods. *)
    method virtual private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b
    method virtual on_initialize : (unit -> unit) -> unit
    method virtual on_shutdown : (unit -> unit) -> unit
    method virtual on_streaming_cycle_start : (int -> unit) -> unit

    (* Method that should be implemented by inheriting classes. *)
    method virtual private start : unit
    method virtual private stop : unit
    val mutable state : state = `Idle
    method state = self#mutexify (fun () -> state) ()
    method stype : Source.source_t = `Fallible

    (* Default [reset] method. Can be overriden if necessary. *)
    method reset =
      self#stop;
      self#start

    method transition_to (s : state) =
      match (s, state) with
        | `Started, `Stopped | `Started, `Idle ->
            self#start;
            on_start ();
            state <- `Started
        | `Started, `Started -> ()
        | `Stopped, `Started ->
            self#stop;
            on_stop ();
            state <- `Stopped
        | `Stopped, `Idle -> state <- `Stopped
        | `Stopped, `Stopped -> ()
        | `Idle, `Started ->
            self#stop;
            on_stop ();
            state <- `Idle
        | `Idle, `Stopped | `Idle, `Idle -> ()

    val mutable is_ready = false
    method is_ready = self#mutexify (fun () -> is_ready) ()

    initializer
    self#on_initialize (fun () -> if autostart then self#transition_to `Started);
    self#on_shutdown (fun () -> self#transition_to `Stopped);
    self#on_streaming_cycle_start (fun _ -> is_ready <- state = `Started)
  end

let proto =
  [
    ( "on_start",
      Lang.fun_t [] Lang.unit_t,
      Some (Lang.val_cst_fun [] Lang.unit),
      Some "Callback executed when input starts." );
    ( "on_stop",
      Lang.fun_t [] Lang.unit_t,
      Some (Lang.val_cst_fun [] Lang.unit),
      Some "Callback executed when input stops." );
    ( "start",
      Lang.bool_t,
      Some (Lang.bool true),
      Some "Start input as soon as it is available." );
  ]

type 'a meth = string * Lang.scheme * string * ('a -> Lang.value)

let meth :
    unit -> < state : state ; transition_to : state -> unit ; .. > meth list =
 fun () ->
  Lang.
    [
      ( "is_started",
        ([], fun_t [] bool_t),
        "`true` if the output or source is started.",
        fun s -> val_fun [] (fun _ -> bool (s#state = `Started)) );
      ( "start",
        ([], fun_t [] unit_t),
        "Ask the source or output to start.",
        fun s ->
          val_fun [] (fun _ ->
              s#transition_to `Started;
              unit) );
      ( "stop",
        ([], fun_t [] unit_t),
        "Ask the source or output to stop.",
        fun s ->
          val_fun [] (fun _ ->
              s#transition_to `Stopped;
              unit) );
    ]
