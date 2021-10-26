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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** In [`CPU] mode, synchronization is governed by the CPU clock.
  * In [`None] mode, there is no synchronization control. Latency in
  * is governed by the time it takes for the sources to produce and
  * output data.
  * In [`Auto] mode, synchronization is governed by the CPU unless at
  * least one active source is declared [self_sync] in which case latency
  * is delegated to this source. A typical example being a source linked
  * to a sound card, in which case the source latency is governed
  * by the sound card's clock. Another case is synchronous network
  * protocol such as [input.srt]. *)
type sync = [ `Auto | `CPU | `None ]

module Time : Liq_time.T

module type Tick = sig
  type 'a t

  val ( >> ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  (* Iter on all values, returning the list of those who
     did not raise an exception and calling [on_error] when
     they do. *)
  val iter :
    on_error:(bt:Printexc.raw_backtrace -> 'a -> exn -> unit) ->
    ('a -> unit t) ->
    'a list ->
    'a list t

  val sleep : float -> unit t
  val exec : (unit -> unit t) -> unit
end

module AsyncTick : Tick
module SyncTick : Tick

module type T = sig
  type 'a tick

  module Tick : Tick with type 'a t := 'a tick

  type source =
    < id : string
    ; clock : t
    ; ctype : Frame.content_type
    ; is_active : bool
    ; self_sync : [ `Static | `Dynamic ] * bool
    ; prepare : unit
    ; negociate : unit
    ; initialize : unit
    ; error : bt:Printexc.raw_backtrace -> exn -> unit
    ; start_streaming_cycle : int -> unit
    ; is_ready : bool
    ; frame : Frame.t
    ; generate_data : unit tick
    ; did_generate_data : bool
    ; end_streaming_cycle : int -> unit
    ; reset : unit
    ; shutdown : unit >

  and t =
    < id : string
    ; sync : sync
    ; add_source : source -> unit
    ; remove_source : source -> unit
    ; time : Time.t
    ; tick : (bool * int) tick
    ; start : unit
    ; stop : unit >

  val make : sync:sync -> id:string -> unit -> t
  val main : t
end

module MkClock (Tick : Tick) : T with type 'a tick := 'a Tick.t
module Async : T with type 'a tick := 'a AsyncTick.t
module Sync : T with type 'a tick := 'a SyncTick.t
include T
