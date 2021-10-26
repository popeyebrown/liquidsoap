(*****************************************************************************

  Liquidsoap, a programmable stream generator.
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

(* For some synthetized function (e.g. noise, blank, sine), we can pretend we
   support seek by doing nothing. However, for other, seek should be
   disabled. Thus, if [seek] is [true], the seek function is [fun x -> x]
   otherwise it is [fun _ -> 0] *)
class virtual source ?name ~seek kind duration =
  let track_size = Option.map Frame.main_of_seconds duration in
  object (self)
    inherit Source.source ?name kind
    method stype = if track_size = None then `Infallible else `Fallible
    val mutable remaining = track_size
    method get_frame_ready = remaining <> Some 0
    method seek x = if seek then x else 0
    method self_sync = (`Static, false)

    method remaining =
      match remaining with None -> -1 | Some remaining -> remaining

    val mutable add_track_mark = false
    method abort_track = self#mutexify (fun () -> add_track_mark <- true) ()
    method virtual private synthesize : length:int -> Frame.t -> unit
    method reset = ()

    method private get_frame ~length frame =
      let len =
        match remaining with
          | None -> Lazy.force Frame.size
          | Some r ->
              let len = min (Lazy.force Frame.size) r in
              remaining <- Some (r - len);
              len
      in
      self#synthesize ~length frame;
      match (add_track_mark, remaining) with
        | true, _ | _, Some 0 ->
            Frame.add_track_mark frame len;
            add_track_mark <- false
        | _, _ -> ()
  end
