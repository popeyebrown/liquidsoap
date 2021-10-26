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

(* Type for source's self_sync. A [`Static] self_sync should never change over
   the source's lifetime. *)
type self_sync = [ `Static | `Dynamic ] * bool

(** The liveness type of a source indicates whether or not it can
  * fail to broadcast.
  * A `Infallible source never fails; it is always ready. *)
type source_t = [ `Fallible | `Infallible ]

type 'a tick = 'a Clock.tick

exception Clock_conflict of (string * string)

(** Generate an identifier from the name of the source. *)
val generate_id : string -> string

class virtual source :
  ?name:string
  -> ?audio_in:Frame.kind
  -> ?video_in:Frame.kind
  -> ?midi_in:Frame.kind
  -> Kind.t
  -> object
       method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b

       (** {1 Naming} *)

       (** Identifier of the source. *)
       method id : string

       method set_name : string -> unit
       method set_id : ?definitive:bool -> string -> unit

       (** {1 Init/shutdown} *)

       (** Does the source provide its own synchronization?
           Examples: Alsa, AO, SRT I/O, etc.. This information
           is used at the clock level to decide whether or not
           we should synchronize with the CPU clock after producing
           a frame (for [`Auto] clocks). Please note that in the case
           of multiple sources filling the frame with different notion
           notion of synchronization, there is no consistent notion
           of time or synchronization. In this case (and with a [`Auto]
           clock), we simply decide based on whether there is one [self_sync]
           source or not. This logic should dictate how the method is
           implemented by the various operators. *)
       method virtual self_sync : self_sync

       (** Active sources are always animated during a streaming cycle. This include outputs but
           also sources such as [input.http]. *)
       method is_active : bool

       (* {1 Content information and negociation} *)

       (** Kind of content produced by the source. This is used during
           content negociation and might be incomplete. *)
       method kind : Kind.t

       (* Complete content-type produced by the source at runtime. *)
       method ctype : Frame.content_type

       (** Assign a clock to this source. *)
       method set_clock : Clock.t -> unit

       (** Get the source's clock. Defaults to [Clock.main] if no clock
           is assigned. *)
       method clock : Clock.t

       (** Convenience wrapper around [self#ctype] to return the number of audio channels on
           sources that produce PCM content. Fails otherwise. *)
       method private audio_channels : int

       (** Source initialization is done in two phases to help finding the best
           content type and unifying resources such as clock:
           - [prepare]
           - [negociate]
           - [initialize] *)

       (** Register a callback to execute when the source needs to prepare stuff. *)
       method on_prepare : (unit -> unit) -> unit

       (** Prepare. In particular, may try to queue a request if local and find out its
           content type? *)
       method prepare : unit

       (** Register a callback to execute when the source needs to negociate stuff. *)
       method on_negociate : (unit -> unit) -> unit

       (** Negociate content type. The source unifies its own content type
          with its children sources during that call. *)
       method negociate : unit

       (** Register a callback to execute when the source has finished initializing. *)
       method on_initialize : (unit -> unit) -> unit

       (** Initialize the source. *)
       method initialize : unit

       (** Register a callback to execute when the source shuts down. *)
       method on_shutdown : (unit -> unit) -> unit

       (** Shutdown the source. *)
       method shutdown : unit

       (** Set a handler to be called when an error occurs during the source's lifetime. *)
       method on_error : (bt:Printexc.raw_backtrace -> exn -> unit) -> unit

       (** Signal an error that occured on the source's lifetime. *)
       method error : bt:Printexc.raw_backtrace -> exn -> unit

       (* {1 Streaming cycle cycle management} *)

       (** Streaming cycles consist of the following calls:
         - [start_streaming_cycle <expected>]: Start one streaming cycle and expect
           the source to produce [expected] main ticks of data or less. Source should
           only produce less ticks on track end or when it's out of data.
         - [is_ready] indicates if a source is available during this streaming cycle
           and should return the same value during the whole cycle.
         - [generate_data]: Generate data for this round. The source _must_ generate
           some data during that call.
         - [did_generate_data]: [true] if the source generated some data during
           the current streaming cycle.
         - [end_streaming_cycle <produced>]: End the current streaming cycle. [produced] is
           the duration of data produced during this cycle in main ticks. If the source
           produced more data, it should keep the remainder and use it during the next
           streaming cycle. *)

       (** Register a callback to be executed before the streaming cycle 
           starts. *)
       method on_before_streaming_cycle_start : (int -> unit) -> unit

       (** Register a callback to be executed when the streaming cycle
           starts. *)
       method on_streaming_cycle_start : (int -> unit) -> unit

       (** Begin a streaming cycle. *)
       method start_streaming_cycle : int -> unit

       (** Register a callback to be executed after the streaming cycle
           starts. *)
       method on_after_streaming_cycle_start : (int -> unit) -> unit

       (** [is_ready] tells you if [generate_data] can be called. [is_ready]
           is constant during a whole streaming cycle. *)
       method is_ready : bool

       (** Each source should implement [fill_frame_ready], to be checked at the 
           beginning of each streaming cycle. Its value is then returned by
           [is_ready] during the whole streaming cycle. *)
       method virtual fill_frame_ready : bool

       (** [stype] is the liveness type, telling whether a scheduler is
           fallible or not, i.e. [is_ready] is always [true].
           It is defined by each operator based on its sources' types. *)
       method virtual stype : [ `Fallible | `Infallible ]

       (** Source status indicates if the source was sucessfully initialized or 
           shutdown. *)
       method status :
         [ `Fresh | `Initialized | `Negociated | `Prepared | `Shutdown ]

       (** Called when there is too much latency. *)
       method virtual reset : unit

       (** Internal frame filled up during each streaming cycle. *)
       method frame : Frame.t

       (** Ask the source to generate its data for the currrent streaming cycle. This method 
           should only be called during a streaming cycle while the source is ready and should
           always produce at least some data. *)
       method generate_data : unit tick

       (** [true] if the source did generate data during the current streaming cycle. *)
       method did_generate_data : bool

       (** Register a callback to be executed when a new track begins. *)
       method on_track : (int -> unit) -> unit

       (** Register a callback to be executed when a new metadata is generated. *)
       method on_metadata : (pos:int -> Frame.metadata -> unit) -> unit

       (** Register a callback to be called before [fill_frame] *)
       method on_before_fill_frame : (Frame.t -> unit tick) -> unit

       (** Internal method actually implementing frame content generation.
           It is called by [generate_data]. *)
       method virtual private fill_frame : length:int -> Frame.t -> unit

       (** Register a callback to be called after [fill_frame] *)
       method on_after_fill_frame : (Frame.t -> unit tick) -> unit

       (** Register a callback to be executed before the streaming cycle ends. *)
       method on_before_streaming_cycle_end : (int -> unit) -> unit

       (** Register a callback to be executed when the streaming cycle ends. *)
       method on_streaming_cycle_end : (int -> unit) -> unit

       (** End a streaming cycle. *)
       method end_streaming_cycle : int -> unit

       (** Register a callback to be executed after the streaming cycle ends. *)
       method on_after_streaming_cycle_end : (int -> unit) -> unit

       (* {1 Dynamic properties} *)

       (** Number of main ticks left in the current track. Defaults to -1=infinity. *)
       method virtual remaining : int

       method elapsed : int
       method duration : int
       method seek : int -> int

       (** Tells the source to finish the reading of current track. *)
       method virtual abort_track : unit

       (** {1 Utilities} *)

       method log : Log.t
     end

(* Leaf sources, which need to be animated by their clock, typically outputs
   and live inputs, e.g. [input.http] *)
and virtual active_source :
  ?name:string
  -> ?audio_in:Frame.kind
  -> ?video_in:Frame.kind
  -> ?midi_in:Frame.kind
  -> Kind.t
  -> object
       inherit source
     end

(* This is for defining a source which has children *)
class virtual operator :
  ?name:string
  -> ?audio_in:Frame.kind
  -> ?video_in:Frame.kind
  -> ?midi_in:Frame.kind
  -> Kind.t
  -> source list
  -> object
       inherit source
     end

(* Most usual active source: the active_operator, pulling one source's data
 * and outputting it. *)
class virtual active_operator :
  ?name:string
  -> ?audio_in:Frame.kind
  -> ?video_in:Frame.kind
  -> ?midi_in:Frame.kind
  -> Kind.t
  -> source list
  -> object
       inherit active_source
     end
