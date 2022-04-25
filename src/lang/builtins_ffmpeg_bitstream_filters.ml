(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

module Generator = Generator.From_audio_video

let log = Log.make ["ffmpeg"; "bitstream"; "filter"]

let () =
  Lang.add_module "ffmpeg.bitstream";
  Lang.add_module "ffmpeg.bitstream.filter"

let args_of_args args =
  let opts = Hashtbl.create 10 in
  let rec f = function
    | [] -> ()
    | `Pair (lbl, v) :: args ->
        Hashtbl.add opts lbl v;
        f args
  in
  f args;
  opts

let modes name (codecs : Avcodec.id list) =
  let has_audio =
    List.exists
      (fun codec -> List.mem (codec :> Avcodec.id) codecs)
      Avcodec.Audio.codec_ids
  in
  let has_video =
    List.exists
      (fun codec -> List.mem (codec :> Avcodec.id) codecs)
      Avcodec.Video.codec_ids
  in
  match (codecs, has_audio, has_video) with
    | [], _, _ | _, true, true -> [`Audio; `Video]
    | _, true, false -> [`Audio_only]
    | _, false, true -> [`Video_only]
    | _, false, false ->
        log#important "No valid mode found for filter %s!" name;
        []

let () =
  List.iter
    (fun ({ Avcodec.BitstreamFilter.name; codecs; options } as filter) ->
      let args, args_parser = Builtins_ffmpeg_filters.mk_options options in
      let modes = modes name codecs in
      if List.length modes > 1 then
        Lang.add_module ("ffmpeg.bitstream.filter." ^ name);
      List.iter
        (fun mode ->
          let name, source_kind =
            match mode with
              | `Audio ->
                  ( name ^ ".audio",
                    Frame.
                      {
                        audio = `Kind Ffmpeg_copy_content.Audio.kind;
                        video = `Any;
                        midi = `Any;
                      } )
              | `Audio_only ->
                  ( name,
                    Frame.
                      {
                        audio = `Kind Ffmpeg_copy_content.Audio.kind;
                        video = `Any;
                        midi = `Any;
                      } )
              | `Video ->
                  ( name ^ ".video",
                    Frame.
                      {
                        audio = `Any;
                        video = `Kind Ffmpeg_copy_content.Video.kind;
                        midi = `Any;
                      } )
              | `Video_only ->
                  ( name,
                    Frame.
                      {
                        audio = `Any;
                        video = `Kind Ffmpeg_copy_content.Video.kind;
                        midi = `Any;
                      } )
          in
          let source_t = Lang.kind_type_of_kind_format source_kind in
          let args_t = ("", Lang.source_t source_t, None, None) :: args in
          Lang.add_operator ~category:`FFmpegFilter
            ("ffmpeg.bitstream.filter." ^ name)
            ~descr:
              ("FFmpeg " ^ name
             ^ " bitstream filter. See ffmpeg documentation for more details.")
            ~flags:[`Extra] args_t ~return_t:source_t
            (fun p ->
              let filter =
                Avcodec.BitstreamFilter.init
                  ~opts:(args_of_args (args_parser p []))
                  filter
              in
              assert false))
        modes)
    Avcodec.BitstreamFilter.filters
