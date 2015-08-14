(* Main -- Test the Getopts module

Author: Michael Grünewald
Date: Sun Aug 10 14:49:52 CEST 2014

Getopts (https://github.com/michipili/getopts)
This file is part of Getopts

Copyright © 2008-2015 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Printf

let rest =
  ref []

let spec = Getopts.spec
  "[-avh][-i int][-f float][-b bool][-s string][--][rest]"
  "Test the Getopts module" [
    Getopts.flag 'a'
      (fun () -> printf "flag a\n") "The a flag.";
    Getopts.flag 'v'
      (fun () -> printf "flag v\n") "The v flag.";
    Getopts.bool 'b'
      (fun b () -> printf "option b = %B\n" b) "The b option.";
    Getopts.char 'c'
      (fun c () -> printf "option c = %C\n" c) "The c option.";
    Getopts.string 's'
      (fun s () -> printf "option s = %S\n" s) "The s option.";
    Getopts.int 'i'
      (fun x () -> printf "option i = %d\n" x) "The i option.";
    Getopts.float 'f'
      (fun x () -> printf "option f = %f\n" x) "The f option.";
  ] (Getopts.queue rest) [
    Getopts.note "Short Note"
      "This is an example of a short note.";
    Getopts.note "Long Note"
      "This is an example of a very long note whose contents spans \
       over several lines.  It really has to be that long, so we \
       will write here as much silly text as needed.\n  \
       And we also need several pragraphs of irrelevant text, but \
       believe me, this has nothing to do with you, just with the example.";
  ]

let () =
  begin
    Getopts.parse_argv spec ();
    List.iter (fun s -> printf "rest = %S\n" s) !rest
  end
