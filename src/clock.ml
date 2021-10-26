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

let log = Log.make ["clock"]

let conf_clock =
  Dtools.Conf.void ~p:(Configure.conf#plug "clock") "Clock settings"

module Time : Liq_time.T = (val !Liq_time.implementation)
open Time

let time_zero = Time.of_float 0.

let () =
  Lifecycle.on_init (fun () ->
      log#important "Using %s implementation for latency control"
        Time.implementation)

let conf_log_delay =
  Dtools.Conf.float
    ~p:(conf_clock#plug "log_delay")
    ~d:1. "How often (in seconds) we should indicate catchup errors."

let conf =
  Dtools.Conf.void ~p:(Configure.conf#plug "root") "Streaming clock settings"

let conf_max_latency =
  Dtools.Conf.float ~p:(conf#plug "max_latency") ~d:60.
    "Maximum latency in seconds"
    ~comments:
      [
        "If the latency gets higher than this value, the outputs will be reset,";
        "instead of trying to catch it up second by second.";
        "The reset is typically only useful to reconnect icecast mounts.";
      ]

type sync = [ `Auto | `CPU | `None ]

let sync_descr = function
  | `Auto -> "auto-sync"
  | `CPU -> "CPU sync"
  | `None -> "no sync"

module type Tick = sig
  type 'a t

  val ( >> ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  val iter :
    on_error:(bt:Printexc.raw_backtrace -> 'a -> exn -> unit) ->
    ('a -> unit t) ->
    'a list ->
    'a list t

  val sleep : float -> unit t
  val exec : (unit -> unit t) -> unit
end

module SyncTick = struct
  type 'a t = 'a

  let ( >> ) a b = b a
  let return x = x

  let iter ~on_error fn l =
    List.fold_left
      (fun cur x ->
        try
          fn x;
          x :: cur
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          on_error ~bt x exn;
          cur)
      [] l

  let sleep d = Time.(sleep (of_float d))
  let n = ref (-1)

  let exec fn =
    incr n;
    ignore (Tutils.create fn () (Printf.sprintf "Clock Thread %d" !n))
end

module AsyncTick = struct
  type task = (Tutils.priority, [ `Delay of float ]) Duppy.Task.task

  type 'a t =
    on_error:(bt:Printexc.raw_backtrace -> exn -> task option) ->
    ('a -> task option) ->
    task option

  let ( >> ) a b ~on_error on_done =
    a ~on_error (fun ret ->
        try b ret ~on_error on_done
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          on_error ~bt exn;
          None)

  let return v ~on_error:_ on_done = on_done v
  let cont = function None -> [] | Some t -> [t]

  let iter ~on_error fn l ~on_error:_ on_done =
    if List.length l = 0 then on_done []
    else (
      let m = Mutex.create () in
      let count = ref (List.length l) in
      let ret = ref [] in
      let finished s =
        Tutils.mutexify m
          (fun () ->
            (match s with None -> () | Some s -> ret := s :: !ret);
            decr count;
            if !count = 0 then on_done !ret else None)
          ()
      in
      let tasks =
        List.map
          (fun x _ ->
            let on_error ~bt exn =
              on_error ~bt x exn;
              finished None
            in
            try cont (fn x ~on_error (fun () -> finished (Some x)))
            with exn ->
              let bt = Printexc.get_raw_backtrace () in
              cont (on_error ~bt exn))
          l
      in
      List.iter
        (fun handler ->
          Duppy.Task.add Tutils.scheduler
            { Duppy.Task.priority = `Blocking; events = [`Delay 0.]; handler })
        tasks;
      None)

  let sleep = function
    | 0. -> return ()
    | d ->
        fun ~on_error:_ fn ->
          Some
            {
              Duppy.Task.priority = `Blocking;
              events = [`Delay d];
              handler = (fun _ -> cont (fn ()));
            }

  let exec fn =
    match
      fn ()
        ~on_error:(fun ~bt exn -> Printexc.raise_with_backtrace exn bt)
        (fun () -> None)
    with
      | None -> ()
      | Some t -> Duppy.Task.add Tutils.scheduler t
end

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

module MkClock (T : Tick) = struct
  open T
  module Tick = T

  type 'a tick = 'a T.t

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

  (* As a general rule below, private methods are meant to be called internally
     and are unprotected. Public methods can be called from any thread and are
     protected. *)
  class clock ~sync ~id () =
    object (self)
      val mutex = Mutex.create ()

      method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b =
        Tutils.mutexify mutex

      method id = id
      method sync : sync = sync
      val mutable current_position = time_zero
      method time = self#mutexify (fun () -> current_position) ()
      val mutable added_sources : source list = []
      val mutable current_sources : source list = []
      val mutable removed_sources : source list = []

      method add_source =
        self#mutexify (fun s -> added_sources <- s :: added_sources)

      method remove_source =
        self#mutexify (fun s ->
            List.iter
              (fun s' -> if s = s' then removed_sources <- s :: removed_sources)
              current_sources)

      val log = Log.make ["clock"; id]
      val mutable stop = false
      method stop = self#mutexify (fun () -> stop <- true) ()

      initializer
      Lifecycle.on_core_shutdown (self#mutexify (fun () -> stop <- true))

      method private iter_sources ~sources fn =
        iter ~on_error:(fun ~bt s exn -> s#error ~bt exn) fn sources

      method private shutdown ~sources =
        let len = List.length sources in
        if len > 0 then log#info "Shuting down %d sources" len;
        self#iter_sources ~sources (fun s -> return s#shutdown) >> fun _ ->
        return ()

      method private prepare ~sources =
        let len = List.length sources in
        if len > 0 then log#info "Initializing %d sources" len;
        self#iter_sources ~sources (fun s -> return s#prepare)

      method private negociate ~sources =
        self#iter_sources ~sources (fun s -> return s#negociate)

      method private initialize ~sources =
        self#iter_sources ~sources (fun s ->
            s#initialize;
            log#info "Source %s initialized with content type: %s and clock: %s"
              s#id
              (Frame.string_of_content_type s#ctype)
              s#clock#id;
            return ())

      method private start_streaming_cycle ~sources =
        let duration = Lazy.force Frame.size in
        self#iter_sources ~sources (fun s ->
            return (s#start_streaming_cycle duration))

      method private generate_data ~sources =
        self#iter_sources ~sources (fun s ->
            if s#is_active && s#is_ready then s#generate_data else return ())

      method private end_streaming_cycle ~sources generated =
        self#iter_sources ~sources (fun s ->
            return (s#end_streaming_cycle generated))

      method tick : (bool * int) tick =
        let n_sources, o_sources =
          self#mutexify (fun () -> (added_sources, removed_sources)) ()
        in
        added_sources <- [];
        removed_sources <- [];

        self#shutdown ~sources:o_sources >> fun () ->
        self#prepare ~sources:n_sources >> fun sources ->
        self#negociate ~sources >> fun sources ->
        self#initialize ~sources >> fun sources ->
        let sources =
          self#mutexify
            (fun () ->
              current_sources <- sources @ current_sources;
              current_sources)
            ()
        in
        self#start_streaming_cycle ~sources >> fun sources ->
        self#generate_data ~sources >> fun sources ->
        let self_sync =
          List.fold_left
            (fun self_sync s ->
              if s#is_active && s#is_ready then snd s#self_sync && self_sync
              else self_sync)
            false sources
        in

        let generated =
          List.fold_left
            (fun cur s ->
              if s#did_generate_data then (
                let generated = Frame.position s#frame in
                match cur with
                  | None -> Some generated
                  | Some v -> Some (min v generated))
              else cur)
            None sources
        in

        assert (generated <> None);
        let generated = Option.get generated in

        current_position <-
          current_position |+| Time.of_float (Frame.seconds_of_main generated);

        self#end_streaming_cycle ~sources generated >> fun sources ->
        current_sources <- sources;
        return (self_sync, generated)

      method private reset =
        self#mutexify
          (fun () ->
            self#iter_sources ~sources:current_sources (fun s -> return s#reset))
          ()

      method start =
        let log_delay = Time.of_float conf_log_delay#get in
        let max_latency = Time.of_float conf_max_latency#get in
        let current_latency = ref time_zero in
        let last_latency_log = ref (time ()) in
        log#important "Streaming loop starts in %s mode" (sync_descr sync);
        let rec loop ?self_sync () =
          let start_time = time () in
          self#tick >> fun (new_self_sync, generated) ->
          let generated_time =
            Time.of_float (Frame.seconds_of_main generated)
          in
          let end_time = time () in
          let spent_time = end_time |-| start_time in
          let self_sync =
            match (self#sync, self_sync, new_self_sync) with
              | `Auto, None, false | `Auto, Some true, false ->
                  log#important "Delegating synchronisation to CPU clock";
                  new_self_sync
              | `Auto, None, true | `Auto, Some false, true ->
                  log#important "Delegating synchronisation to active sources";
                  new_self_sync
              | `Auto, _, _ -> new_self_sync
              | `CPU, _, _ -> true
              | `None, _, _ -> false
          in
          let delay =
            match (self_sync, spent_time |-| generated_time) with
              | false, _ -> return 0.
              | true, remaining_time when time_zero |<=| remaining_time ->
                  current_latency := time_zero;
                  last_latency_log := end_time;
                  return (Time.to_float remaining_time)
              | true, remaining_time ->
                  current_latency := !current_latency |-| remaining_time;
                  last_latency_log := !last_latency_log |-| remaining_time;
                  if max_latency |<=| !current_latency then (
                    log#severe "Too much latency! Resetting sources...";
                    current_latency := time_zero;
                    self#reset >> fun sources ->
                    current_sources <- sources;
                    return 0.)
                  else (
                    if log_delay |<=| !last_latency_log then (
                      last_latency_log := end_time;
                      log#severe "We must catchup %.2f seconds!"
                        (Time.to_float !current_latency));
                    return 0.)
          in
          delay >> T.sleep >> loop ~self_sync
        in
        let loop ?self_sync () =
          let stop = self#mutexify (fun () -> stop) () in
          if stop then return () else loop ?self_sync ()
        in
        Tick.exec (loop ?self_sync:None)
    end

  let make ~sync ~id () = new clock ~sync ~id ()
  let main = make ~sync:`CPU ~id:"main" ()
end

module Async = MkClock (AsyncTick)
module Sync = MkClock (SyncTick)

let clock_module =
  match Sys.getenv_opt "LIQ_ASYNC_CLOCK" with
    | None -> (module Sync : T)
    | Some _ -> (module Async : T)

module Impl = (val clock_module : T)
include Impl
