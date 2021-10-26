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

open Mm

(** Output using ao lib. *)

open Ao

(** As with ALSA (even more maybe) it would be better to have one clock
  * per driver... but it might also depend on driver options. *)
let clock = Clock.make ~id:"ao" ~sync:`None ()

let () = Lifecycle.on_start (fun () -> clock#start)

class output ~kind ~nb_blocks ~driver ~on_start ~on_stop ~options
  ?channels_matrix source start =
  let samples_per_frame = AFrame.size () in
  let samples_per_second = Lazy.force Frame.audio_rate in
  let bytes_per_sample = 2 in
  object (self)
    inherit
      Output.output
        ~content_kind:kind ~on_start ~on_stop ~name:"ao"
          ~output_kind:"output.ao" source start

    inherit [Bytes.t] IoRing.output ~nb_blocks as ioring
    val mutable device = None

    initializer
    self#on_initialize (fun () ->
        let blank () =
          Bytes.make
            (samples_per_frame * self#audio_channels * bytes_per_sample)
            '0'
        in
        ioring#init blank);
    self#on_start (fun () ->
        let driver =
          if driver = "" then get_default_driver () else find_driver driver
        in
        let dev =
          self#log#important "Opening %s (%d channels)..." driver.Ao.name
            self#audio_channels;
          open_live ~driver ~options ?channels_matrix ~rate:samples_per_second
            ~bits:(bytes_per_sample * 8) ~channels:self#audio_channels ()
        in
        device <- Some dev);
    self#on_stop (fun () ->
        match device with
          | Some d ->
              Ao.close d;
              device <- None
          | None -> ())

    method self_sync = (`Dynamic, device <> None)

    method push_block data =
      play (Option.get device) (Bytes.unsafe_to_string data)

    method send_frame wav length =
      let push data =
        let pcm = Audio.sub (AFrame.pcm wav) 0 length in
        assert (Array.length pcm = self#audio_channels);
        Audio.S16LE.of_audio pcm data 0
      in
      ioring#put_block push
  end

let () =
  let kind = Lang.audio_pcm in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "output.ao"
    (Output.proto
    @ [
        ( "driver",
          Lang.string_t,
          Some (Lang.string ""),
          Some "Driver to be used, \"\" for AO's default." );
        ( "channels_matrix",
          Lang.string_t,
          Some (Lang.string ""),
          Some "Output channels matrix, \"\" for AO's default." );
        ( "buffer_size",
          Lang.int_t,
          Some (Lang.int 2),
          Some "Set buffer size, in frames." );
        ( "options",
          Lang.metadata_t,
          Some (Lang.list []),
          Some "List of parameters, depends on the driver." );
        ("", Lang.source_t return_t, None, None);
      ])
    ~clock ~category:`Output ~meth:Output.meth
    ~descr:"Output stream to local sound card using libao." ~return_t
    (fun p ->
      let driver = Lang.to_string (List.assoc "driver" p) in
      let nb_blocks = Lang.to_int (List.assoc "buffer_size" p) in
      let options =
        List.map
          (fun x ->
            let a, b = Lang.to_product x in
            (Lang.to_string a, Lang.to_string b))
          (Lang.to_list (List.assoc "options" p))
      in
      let channels_matrix = Lang.to_string (List.assoc "channels_matrix" p) in
      let channels_matrix =
        if channels_matrix = "" then None else Some channels_matrix
      in
      let start = Lang.to_bool (List.assoc "start" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply f [])
      in
      let source = List.assoc "" p in
      let kind = Kind.of_kind kind in
      (new output
         ~kind ~nb_blocks ~driver ~on_start ~on_stop ?channels_matrix ~options
         source start
        :> Output.output))
