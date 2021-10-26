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

type self_sync = [ `Static | `Dynamic ] * bool
type source_t = [ `Fallible | `Infallible ]
type 'a tick = 'a Clock.tick

open Clock.Tick

let debug = Utils.getenv_opt "LIQUIDSOAP_DEBUG" <> None

(** {1 Sources} *)

let source_log = Log.make ["source"]

exception Clock_conflict of (string * string)

(** Generate an identifier from the name of the source. *)
let generate_id =
  let t = Hashtbl.create 10 in
  fun name ->
    if not (Hashtbl.mem t name) then Hashtbl.add t name (ref (-1));
    let n = Hashtbl.find t name in
    incr n;
    name ^ "_" ^ string_of_int !n

class virtual operator ?(name = "src") ?audio_in ?video_in ?midi_in out_kind
  sources =
  let f kind (fn, el) = match el with None -> kind | Some v -> fn kind v in
  let in_kind =
    List.fold_left f out_kind
      [
        (Kind.set_audio, audio_in);
        (Kind.set_video, video_in);
        (Kind.set_midi, midi_in);
      ]
  in
  object (self)

    (** Logging and identification *)

    val mutable log = source_log
    method private create_log = log <- Log.make [self#id]
    method log = log
    val mutable id = ""
    val mutable definitive_id = false
    val mutable name = name
    method set_name n = name <- n
    method id = id

    method set_id ?(definitive = true) s =
      let s = Pcre.substitute ~pat:"[ \t\n]" ~subst:(fun _ -> "_") s in
      if not definitive_id then (
        id <- s;
        definitive_id <- definitive);

      (* Sometimes the ID is changed during initialization,
       * in order to make it equal to the server name,
       * which is only registered at initialization time in order
       * to avoid bloating from unused sources.
       * If the ID changes, and [log] has already been initialized, reset it. *)
      if log != source_log then self#create_log

    initializer
    id <- generate_id name;
    if log == source_log then self#create_log;
    (* TODO *)
    Server.register_op self#id name;
    if debug then
      Gc.finalise (fun s -> source_log#info "Garbage collected %s." s#id) self

    val mutex = Mutex.create ()

    method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b =
      Tutils.mutexify mutex

    (** Is the source active *)
    method is_active = false

    (** Children sources *)
    val mutable sources : operator list = sources

    method virtual self_sync : self_sync
    method kind = out_kind
    method virtual stype : [ `Fallible | `Infallible ]

    val mutable status
        : [ `Fresh | `Prepared | `Negociated | `Initialized | `Shutdown ] =
      `Fresh

    method status = self#mutexify (fun () -> status) ()
    val mutable ctype = None

    (* Content type. *)
    method ctype =
      self#mutexify
        (fun () ->
          assert (List.mem status [`Negociated; `Initialized]);
          match ctype with
            | Some ctype -> ctype
            | None ->
                let ct = Kind.content_type self#kind in
                self#log#debug "Content kind: %s, content type: %s"
                  (Kind.to_string self#kind)
                  (Frame.string_of_content_type ct);
                ctype <- Some ct;
                ct)
        ()

    method private audio_channels =
      Content.Audio.channels_of_format self#ctype.Frame.audio

    val mutable clock : Clock.t option = None

    method set_clock c =
      self#mutexify
        (fun () ->
          match clock with
            | Some c' -> if c <> c' then raise (Clock_conflict (c#id, c'#id))
            | None ->
                c#add_source (self :> Clock.source);
                clock <- Some c)
        ()

    method clock : Clock.t =
      self#mutexify
        (fun () ->
          match clock with
            | Some c -> c
            | None ->
                Clock.main#add_source (self :> Clock.source);
                clock <- Some Clock.main;
                Clock.main)
        ()

    initializer
    Lifecycle.before_start (fun () ->
        List.iter (fun s -> s#set_clock self#clock) sources)

    val mutable on_prepare = []
    method on_prepare = self#mutexify (fun fn -> on_prepare <- fn :: on_prepare)

    method prepare =
      List.iter
        (fun fn -> fn ())
        (self#mutexify
           (fun () ->
             assert (status = `Fresh);
             on_prepare)
           ());
      status <- `Prepared

    val mutable on_negociate = []

    method on_negociate =
      self#mutexify (fun fn -> on_negociate <- fn :: on_negociate)

    initializer
    self#on_negociate (fun () ->
        List.iter (fun s -> Kind.unify s#kind in_kind) sources)

    method negociate =
      List.iter
        (fun fn -> fn ())
        (self#mutexify
           (fun () ->
             assert (status = `Prepared);
             on_negociate)
           ());
      status <- `Negociated

    val mutable on_initialize = []

    method on_initialize =
      self#mutexify (fun fn -> on_initialize <- fn :: on_initialize)

    method initialize =
      List.iter
        (fun fn -> fn ())
        (self#mutexify
           (fun () ->
             assert (status = `Negociated);
             on_initialize)
           ());
      status <- `Initialized

    val mutable on_shutdown = []

    method on_shutdown =
      self#mutexify (fun fn -> on_shutdown <- fn :: on_shutdown)

    method shutdown =
      List.iter
        (fun fn -> fn ())
        (self#mutexify
           (fun () ->
             assert (status = `Initialized);
             on_shutdown)
           ())

    val mutable on_error = fun ~bt exn -> Printexc.raise_with_backtrace exn bt
    method on_error = self#mutexify (fun fn -> on_error <- fn)
    method error ~bt exn = self#mutexify (fun () -> on_error ~bt exn) ()
    method virtual reset : unit
    val mutable is_ready = false
    method is_ready = self#mutexify (fun () -> is_ready) ()
    method virtual fill_frame_ready : bool
    val mutable on_before_streaming_cycle_start = []

    method on_before_streaming_cycle_start =
      self#mutexify (fun fn ->
          on_before_streaming_cycle_start <-
            fn :: on_before_streaming_cycle_start)

    val mutable on_streaming_cycle_start = []

    method on_streaming_cycle_start =
      self#mutexify (fun fn ->
          on_streaming_cycle_start <- fn :: on_streaming_cycle_start)

    val mutable target_length = 0

    initializer
    self#on_streaming_cycle_start (fun length ->
        let frame = self#frame in
        target_length <- length;
        is_ready <- Frame.position frame > 0 || self#fill_frame_ready)

    val mutable on_after_streaming_cycle_start = []

    method on_after_streaming_cycle_start =
      self#mutexify (fun fn ->
          on_after_streaming_cycle_start <- fn :: on_after_streaming_cycle_start)

    method start_streaming_cycle duration =
      List.iter
        (fun fn -> fn duration)
        (self#mutexify
           (fun () ->
             on_before_streaming_cycle_start @ on_streaming_cycle_start
             @ on_after_streaming_cycle_start)
           ())

    val mutable on_before_fill_frame = []

    method on_before_fill_frame =
      self#mutexify (fun fn ->
          on_before_fill_frame <- fn :: on_before_fill_frame)

    val mutable on_after_fill_frame = []

    method on_after_fill_frame =
      self#mutexify (fun fn -> on_after_fill_frame <- fn :: on_after_fill_frame)

    (* TODO: move these to the scripting level. *)
    method on_track fn =
      self#on_after_fill_frame (fun frame ->
          List.iter fn (Frame.track_marks frame);
          return ())

    method on_metadata fn =
      self#on_after_fill_frame (fun frame ->
          List.iter (fun (pos, m) -> fn ~pos m) (Frame.get_all_metadata frame);
          return ())

    method private get frame =
      let fold =
        List.fold_left (fun cur fn -> cur >> fun () -> fn frame) (return ())
      in
      fold on_before_fill_frame >> fun () ->
      self#fill_frame ~length:target_length frame;
      assert (Frame.position frame > 0);
      return () >> fun () -> fold on_after_fill_frame

    val mutable did_generate_data = false

    initializer
    self#on_streaming_cycle_start (fun _ -> did_generate_data <- false)

    method did_generate_data = self#mutexify (fun () -> did_generate_data) ()

    method generate_data =
      let frame = self#frame in
      assert self#is_ready;
      did_generate_data <- true;
      if Frame.is_empty frame then self#get frame else return ()

    val mutable on_before_streaming_cycle_end = []

    method on_before_streaming_cycle_end =
      self#mutexify (fun fn ->
          on_before_streaming_cycle_end <- fn :: on_before_streaming_cycle_end)

    val mutable on_streaming_cycle_end = []

    method on_streaming_cycle_end =
      self#mutexify (fun fn ->
          on_streaming_cycle_end <- fn :: on_streaming_cycle_end)

    initializer
    self#on_streaming_cycle_end (fun len -> Frame.advance ~len self#frame)

    val mutable on_after_streaming_cycle_end = []

    method on_after_streaming_cycle_end =
      self#mutexify (fun fn ->
          on_after_streaming_cycle_end <- fn :: on_after_streaming_cycle_end)

    method end_streaming_cycle produced =
      List.iter
        (fun fn -> fn produced)
        (self#mutexify
           (fun () ->
             on_before_streaming_cycle_end @ on_streaming_cycle_end
             @ on_after_streaming_cycle_end)
           ())

    (** Dyamic properties *)

    (* Number of frames left in the current track:
     * -1 means Infinity, time unit is the frame. *)
    method virtual remaining : int
    val mutable elapsed = 0
    method elapsed = elapsed

    method duration =
      let r = self#remaining in
      let e = self#elapsed in
      if r < 0 || e < 0 then -1 else e + r

    (* [self#seek x] skips [x] main ticks.
     * returns the number of ticks actually skipped.
     * By default it always returns 0, refusing to seek at all. *)
    method seek (_ : int) =
      self#log#important "Seek not implemented!";
      0

    (* If possible, end the current track.
     * Typically, that signal is just re-routed, or makes the next file
     * to be played if there's anything like a file. *)
    method virtual abort_track : unit

    (* In caching mode, remember what has been given during the current
       tick. The generation is deferred until we actually have computed the kind
       by unfication. *)
    val mutable frame = None

    method frame =
      let ctype = self#ctype in
      self#mutexify
        (fun () ->
          match frame with
            | Some frame -> frame
            | None ->
                let m = Frame.create ctype in
                frame <- Some m;
                m)
        ()

    method virtual private fill_frame : length:int -> Frame.t -> unit
  end

(** Entry-point sources, which need to actively perform some task. *)
and virtual active_operator ?name ?audio_in ?video_in ?midi_in content_kind
  sources =
  object
    inherit operator ?name ?audio_in ?video_in ?midi_in content_kind sources
    method is_active = true
  end

(** Shortcuts for defining sources with no children *)

class virtual source ?name ?audio_in ?video_in ?midi_in content_kind =
  object
    inherit operator ?name ?audio_in ?video_in ?midi_in content_kind []
  end

class virtual active_source ?name ?audio_in ?video_in ?midi_in content_kind =
  object
    inherit active_operator ?name ?audio_in ?video_in ?midi_in content_kind []
  end
