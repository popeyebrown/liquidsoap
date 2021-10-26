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

(** Abstract classes for easy creation of output nodes. *)

open Source

let proto =
  Start_stop.proto
  @ [
      ( "fallible",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Allow the child source to fail, in which case the output will be \
           stopped until the source is available again." );
    ]

let meth = Start_stop.meth ()

(** Given abstract start stop and send methods, creates an output.
  * Takes care of pulling the data out of the source, type checkings,
  * maintains a queue of last ten metadata and setups standard Server commands,
  * including start/stop. *)
class virtual output ~content_kind ~output_kind ?(name = "")
  ~(on_start : unit -> unit) ~(on_stop : unit -> unit) val_source autostart =
  let source = Lang.to_source val_source in
  object (self)
    inherit active_operator ~name:output_kind content_kind [source]
    inherit Start_stop.base ~on_start ~on_stop ~autostart () as start_stop
    method virtual private send_frame : Frame.t -> int -> unit
    method self_sync = source#self_sync
    val mutable on_start = [on_start]
    method on_start = self#mutexify (fun fn -> on_start <- fn :: on_start)

    method private start =
      List.iter (fun fn -> fn ()) (self#mutexify (fun () -> on_start) ())

    val mutable on_stop = [on_stop]
    method on_stop = self#mutexify (fun fn -> on_stop <- fn :: on_stop)

    method private stop =
      List.iter (fun fn -> fn ()) (self#mutexify (fun () -> on_stop) ())

    (* Registration of Telnet commands must be delayed because some operators
       change their id at initialization time. *)
    val mutable registered_telnet = false

    method private register_telnet =
      if not registered_telnet then (
        registered_telnet <- true;
        (* Add a few more server controls *)
        let ns = [self#id] in
        Server.add ~ns "skip"
          (fun _ ->
            self#skip;
            "Done")
          ~descr:"Skip current song.";
        Server.add ~ns "metadata" ~descr:"Print current metadata." (fun _ ->
            let q = self#metadata_queue in
            fst
              (Queue.fold
                 (fun (s, i) m ->
                   let s =
                     s
                     ^ (if s = "" then "--- " else "\n--- ")
                     ^ string_of_int i ^ " ---\n"
                     ^ Request.string_of_metadata m
                   in
                   (s, i - 1))
                 ("", Queue.length q)
                 q));
        Server.add ~ns "remaining" ~descr:"Display estimated remaining time."
          (fun _ ->
            let r = source#remaining in
            if r < 0 then "(undef)"
            else (
              let t = Frame.seconds_of_main r in
              Printf.sprintf "%.2f" t)))

    method remaining = source#remaining
    method abort_track = source#abort_track
    method seek len = source#seek len

    (* Operator startup *)
    initializer
    self#on_initialize (fun () ->
        (* We prefer [name] as an ID over the default,
         * but do not overwrite user-defined ID.
         * Our ID will be used for the server interface. *)
        if name <> "" then self#set_id ~definitive:false name;

        self#register_telnet)

    (* Metadata stuff: keep track of what was streamed. *)
    val q_length = 10
    val metadata_q = Queue.create ()

    method private add_metadata m =
      Queue.add m metadata_q;
      if Queue.length metadata_q > q_length then ignore (Queue.take metadata_q)

    method private metadata_queue = Queue.copy metadata_q
    val mutable skip = false
    method private skip = self#mutexify (fun () -> skip <- true) ()

    initializer
    self#on_before_get_frame (fun _ -> source#generate_data);
    self#on_before_streaming_cycle_end (fun generated ->
        (* Output the source's frame if it has some data *)
        if generated > 0 then self#send_frame source#frame generated);
    self#on_after_streaming_cycle_end (fun _ ->
        (* Perform skip if needed *)
        if skip then (
          self#log#important "Performing user-requested skip";
          skip <- false;
          self#abort_track))

    method private get_frame_ready =
      if source#get_frame_ready && state <> `Stopped then
        start_stop#transition_to `Started;
      source#get_frame_ready && state = `Started

    method private get_frame frame =
      Frame.blit source#frame 0 frame 0 (Frame.position source#frame);
      List.iter
        (fun (_, m) -> self#add_metadata m)
        (Frame.get_all_metadata frame)
  end

class dummy ~on_start ~on_stop ~autostart ~kind source =
  object
    inherit
      output
        source autostart ~name:"dummy" ~output_kind:"output.dummy" ~on_start
          ~on_stop ~content_kind:kind

    method private send_frame _ _ = ()
  end

let () =
  let kind = Lang.any in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "output.dummy"
    (proto @ [("", Lang.source_t return_t, None, None)])
    ~category:`Output ~descr:"Dummy output for debugging purposes." ~meth
    ~return_t
    (fun p ->
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let on_start = List.assoc "on_start" p in
      let on_stop = List.assoc "on_stop" p in
      let on_start () = ignore (Lang.apply on_start []) in
      let on_stop () = ignore (Lang.apply on_stop []) in
      let kind = Kind.of_kind kind in
      new dummy ~kind ~on_start ~on_stop ~autostart (List.assoc "" p))

(** More concrete abstract-class, which takes care of the #send_frame
  * method for outputs based on encoders. *)
class virtual encoded ~content_kind ~output_kind ~name ~on_start ~on_stop
  ~autostart source =
  object (self)
    inherit
      output
        ~on_start ~on_stop ~content_kind ~output_kind ~name source autostart

    method virtual private insert_metadata : Meta_format.export_metadata -> unit
    method virtual private encode : Frame.t -> int -> int -> 'a
    method virtual private send : 'a -> unit

    method private send_frame frame length =
      let rec output_chunks frame =
        let f start stop =
          begin
            match Frame.get_metadata frame start with
            | None -> ()
            | Some m -> self#insert_metadata (Meta_format.export_metadata m)
          end;
          let data = self#encode frame start (stop - start) in
          self#send data
        in
        function
        | [] -> assert false
        | [i] -> assert (i = length)
        | start :: stop :: l ->
            f start stop;
            output_chunks frame (stop :: l)
      in
      output_chunks frame
        (List.sort_uniq compare
           (List.map fst (Frame.get_all_metadata frame)
           @ Frame.track_marks frame)
        @ [0; length])
  end
