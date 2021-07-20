/** gzip IO */;

let input = io => {
  let iz = Gzip_stream.open_in(io);
  IO.create_in(
    ~read=() => Gzip_stream.input_char(iz),
    ~input=Gzip_stream.input(iz),
    ~close=() => Gzip_stream.close_in(iz),
  );
};

let output = io => {
  let oz = Gzip_stream.open_out(io);
  IO.create_out(
    ~write=Gzip_stream.output_char(oz),
    ~output=
      (s, o, l) => {
        Gzip_stream.output(oz, s, o, l);
        l;
      },
    ~flush=() => IO.flush(io),
    ~close=() => Gzip_stream.close_out(oz),
  );
};

let input_ch = ch => input(IO.input_channel(ch));
let output_ch = ch => output(IO.output_channel(ch));

/*
 let pipe_in f =
   bracket (Filename.open_temp_file ~mode:[Open_binary] "gzip_io" "gz")
     (fun (tmpname,ch) -> close_out_noerr ch; Sys.remove tmpname)
     (fun (tmpname,ch) ->
       bracket (output_ch ch) (suppress IO.close_out) (fun out ->
         f out;
         IO.close_out out;
         Std.input_file ~bin:true tmpname
       )
     )
 */

let string = s => {
  let out = output(IO.output_string());
  IO.nwrite(out, Bytes.unsafe_of_string(s)); /* IO wrong type */
  IO.close_out(out);
};

let to_string = s => {
  let inp = input(IO.input_string(s));
  let out = IO.output_string();
  try(
    {
      while (true) {
        IO.write(out, IO.read(inp));
      };
      assert(false);
    }
  ) {
  | IO.No_more_input =>
    IO.close_in(inp);
    IO.close_out(out);
  };
};
