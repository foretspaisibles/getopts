(* Getopts -- Program arguments analysis

   Author: Michael Grünewald
   Date: Sun  4 May 2008 11:08:10 CEST

   Copyright © 2008-2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Printf

let progname () =
  Filename.basename Sys.executable_name

module Success =
  Lemonade_Success.Make(String)


(* Error messages *)

let error fmt =
  ksprintf Success.error fmt

let error_illegal_short c =
  error "illegal option -- %s" (Char.escaped c)

let error_illegal_long c opt =
  error "illegal option -- %s %S" (Char.escaped c) opt

let error_argument_short c =
  error "option requires an argument -- %s" (Char.escaped c)

let error_argument_long c name =
  error "option requires an argument -- %s %s" (Char.escaped c) name

let error_separator () =
  error "illegal double dash construction"

let error_convert_short c opt mesg =
  error "invalid argument -- %s %S: %s" (Char.escaped c) opt mesg

let error_convert_long c name opt mesg =
  error "invalid argument -- %s %s %S: %s" (Char.escaped c) name opt mesg


(* Analyse of the command line *)

module Parser =
struct
  type token =
    | Flag of char
    | Option of char * string
    | Rest of string

  let string_to_list s =
    Array.init (String.length s) (String.get s)
    |> Array.to_list

  let string_of_list lst =
    let a = Array.of_list lst in
    String.init (Array.length a) (Array.get a)

  let wordlist_of_argv argv =
    let rec flatten wordlist =
      let embed lst =
        List.map (fun x -> Some(x)) lst
      in
      match wordlist with
      | w1 :: w2 :: t -> (embed w1)@(None::flatten(w2::t))
      | w :: [] -> embed w
      | [] -> []
    in
    let streamdata =
      Array.to_list argv
      |> List.map string_to_list
      |> flatten
    in
    streamdata

  (* Read the word list until the next gap represented by [None]
     or the end of the stream. *)
  let rec wordlist_readword buffer words =
    match words with
    | [] -> (string_of_list (List.rev buffer), [])
    | None :: tl -> (string_of_list (List.rev buffer), tl)
    | Some c :: tl -> wordlist_readword (c :: buffer) tl

  type state = {
    stream: char option list;
    buffer: char list;
    answer: token list;
  }

  let pack_flag c state =
    Success.return { state with answer = Flag(c) :: state.answer }

  let pack_option c (arg, state) =
    Success.return { state with answer = Option(c, arg) :: state.answer }

  let pack_rest (s, state) =
    Success.return { state with answer = Rest(s) :: state.answer }

  let add c state =
    Success.return { state with buffer = c :: state.buffer }

  let junk state =
    match state.stream with
    | _ :: tl -> Success.return({ state with stream = tl })
    | [] -> error "Getopts.Parser.junk"

  let slurp state =
    let word, stream = wordlist_readword state.buffer state.stream in
    Success.return (word, { state with buffer = []; stream })

  let argv_parser : string -> string -> string array ->
    (token list) Success.t =
    fun flags options argv ->
      let open Success.Infix in
      let is_flag c = String.contains flags c in
      let is_option c = String.contains options c in
      let rec maybe_read_cluster state =
        match state.stream with
        | Some('-') :: _ -> junk state >>= read_cluster_enter
        | Some(c) :: _ -> junk state >>= add c >>= read_trail
        | None :: _ -> pack_rest("", state) >>= junk >>= read_rest
        | [] -> Success.return state
      and read_trail state =
        slurp state >>= pack_rest >>= read_rest
      and read_rest state =
        match state.stream with
        | [] -> Success.return state
        | _ -> read_trail state
      and read_cluster_enter state =
        match state.stream with
        | None :: _ -> pack_rest("-", state) >>= junk >>= read_rest
        | Some('-') :: _ -> junk state >>= read_end_of_separator
        | Some(c) :: _ -> junk state >>= read_cluster_option c
        | [] -> pack_rest("-", state)
      and read_cluster_continue state =
        match state.stream with
        | None :: _ -> junk state >>= maybe_read_cluster
        | Some('-') :: _ -> pack_rest("-", state) >>= junk >>= read_rest
        | Some(c) :: _ -> junk state >>= read_cluster_option c
        | [] -> Success.return state
      and read_cluster_option c state =
        if is_flag c then
          pack_flag c state >>= read_cluster_continue
        else if is_option c then
          read_option_argument c state
        else
          error_illegal_short c
      and read_option_argument c state =
        match state.stream with
        | [] -> error_argument_short c
        | None :: None :: _ ->
            pack_option c ("", state) >>= junk >>= junk >>= maybe_read_cluster
        | None :: tl -> junk state >>= read_option_argument c
        | _ -> slurp state >>= pack_option c >>= maybe_read_cluster
      and read_end_of_separator state =
        match state.stream with
        | None :: _ -> junk state >>= read_rest
        | [] -> Success.return state
        | _ -> error_separator ()
      in
      maybe_read_cluster {
        stream = wordlist_of_argv argv;
        buffer = [];
        answer = [];
      } >>= (fun state -> Success.return(List.rev state.answer))
end

  (* Traditional flags and options *)

type 'a t = {
  option: char;
  help: string;
  edit: string -> ('a -> 'a) Success.t;
  wants_arg: bool;
}

type note = {
  title: string;
  content: string;
}

type 'a spec = {
  usage: string;
  description: string;
  options: 'a t list;
  rest: string -> 'a -> 'a;
  notes: note list;
}

let xmap get set option =
  let open Success.Infix in
  let edit s =
    option.edit s
    >>= fun inner_edit ->
    Success.return(fun x -> (set @@ inner_edit @@ get @@ x) x)
  in { option with edit }

let convert_short c of_string s =
  try Success.return(of_string s)
  with
  | Invalid_argument(mesg)
  | Failure(mesg) -> error_convert_short c s mesg
  | _ -> error "Getopts.convert_short"
(* It is not permitted to of_string to throw something other than a
   failure or an invalid argument. *)

let flag option edit0 help = {
  option;
  help;
  edit = (fun _ -> Success.return(edit0));
  wants_arg = false;
}

let option of_string c edit0 help = {
  option = c;
  help;
  edit = (fun s -> Success.Infix.(edit0 <$> convert_short c of_string s));
  wants_arg = true;
}

let char_of_string s =
  if String.length s != 1 then
    failwith "char_of_string"
  else
    s.[0]

let char x =
  option char_of_string x

let bool x =
  option bool_of_string x

let string x =
  option (function x -> x) x

let int x =
  option int_of_string x

let float x =
  option float_of_string x

let note title content = {
  title;
  content;
}

let spec usage description options rest notes = {
  usage;
  description;
  options;
  rest;
  notes;
}


(* Preparation of help messages *)

module Message =
struct
  open Format

  let alinea_open title =
    print_cut ();
    open_hvbox 1;
    print_string title;
    print_char ':';
    print_space ()

  let alinea_close () =
    close_box ()

  let paragraph_open () =
    open_box 0

  let paragraph_close () =
    close_box ()

  let paragraph_switch () =
    paragraph_close ();
    print_cut ();
    paragraph_open ()

  let paragraph_insert text =
    let l = String.length text in
    for i = 0 to l - 1 do
      match text.[i] with
      | '\n' -> paragraph_switch ()
      | ' ' -> print_space ()
      | c -> print_char c
    done

  let compose_note note =
    alinea_open note.title;
    paragraph_open ();
    paragraph_insert note.content;
    paragraph_close ();
    alinea_close ()

  let usage_open () =
    open_hovbox 1;
    print_string "Usage:";
    print_space ()

  let usage_close () =
    close_box ();
    print_cut ()

  let usage_open_bracket () =
    print_cut();
    open_hbox ();
    print_char '['

  let usage_close_bracket () =
    print_char ']';
    close_box ();
    print_cut ()

  let usage_insert text =
    let l = String.length text in
    for i = 0 to l - 1 do
      match text.[i] with
      | '\n' -> failwith "usage_insert_text: newlines are not allowed here"
      | ' ' -> print_space ()
      | '[' -> usage_open_bracket ()
      | ']' -> usage_close_bracket ()
      | c -> print_char c
    done

  let compose_usage usage description =
    usage_open ();
    usage_insert usage;
    usage_close ();
    open_hbox ();
    print_char ' ';
    print_string description;
    close_box ();
    print_cut ()

  let option_open option =
    print_cut();
    open_vbox 3;
    print_char '-';
    print_char option;
    print_char ' '

  let option_close () =
    close_box()

  let option_newline () =
    close_box ();
    print_cut ();
    open_box 0

  let option_insert text =
    let l = String.length text in
    for i = 0 to l - 1 do
      match text.[i] with
      | '\n' -> option_newline ()
      | ' ' -> print_space ()
      | c -> print_char c
    done

  let compose_option option =
    option_open option.option;
    open_box 0;
    option_insert option.help;
    close_box ();
    option_close()

  (* Prepare the help message associated to the given spec. *)
  let help spec =
    open_vbox 0;
    compose_usage (progname () ^ " " ^ spec.usage) spec.description;
    open_vbox 1;
    print_string "Options:";
    List.iter compose_option
      (List.sort (fun a b -> Pervasives.compare a.option b.option)
         (List.filter (fun opt -> opt.help <> "") spec.options));
    close_box();
    List.iter compose_note spec.notes;
    print_cut();
    close_box()

  (* Prepare the usage message associated to the given spec. *)
  let usage spec =
    Format.set_formatter_out_channel stderr;
    open_vbox 0;
    usage_open ();
    usage_insert (progname () ^ " " ^ spec.usage);
    usage_close ();
    close_box()
end


module OptionList =
struct

  let _option_has_char c x =
    x.option = c

  let is_option lst c =
    List.exists (_option_has_char c) lst

  let _lookup lst c =
    try Success.return (List.find (_option_has_char c) lst).edit
    with Not_found -> error_illegal_short c

  let edit spec tok =
    let open Success.Infix in
    let open Parser in
    match tok with
    | Flag(c) -> (_lookup spec.options c) >>= (fun f -> f "")
    | Option(c, optarg) -> (_lookup spec.options c) >>= (fun f -> f optarg)
    | Rest(s) -> Success.return(spec.rest s)

  let _set predicate lst =
    List.filter predicate lst
    |> List.map (fun x -> x.option)
    |> Array.of_list
    |> (fun x -> String.init (Array.length x) (Array.get x))

  let flags lst =
    _set (fun x -> not(x.wants_arg)) lst

  let options lst =
    _set (fun x -> x.wants_arg) lst
end

let help_flag = {
  option = 'h';
  wants_arg = false;
  help = "Display available options.";
  edit = (fun _ -> error "")
}

let maybe_add_help spec =
  if OptionList.is_option spec.options 'h' then
    spec
  else
    { spec with options = help_flag :: spec.options }

let help spec =
  Message.help spec;
  exit 0

let usage spec mesg =
  eprintf "%s: %s\n" (progname()) mesg;
  Message.usage spec;
  exit 64 (* See sysexits(3) *)


let parse argv spec0 default =
  let open Success.Infix in
  let spec = maybe_add_help spec0 in
  let compose lst =
    List.fold_left ( |> ) default lst
  in
  let success =
    (Parser.argv_parser
       (OptionList.flags spec.options)
       (OptionList.options spec.options)
       argv)
    >>= (fun lst -> (Success.dist(List.map (OptionList.edit spec) lst)))
    >>= (fun lst -> (Success.return (compose lst)))
  in
  match Success.run success with
  | Success.Success(param) -> param
  | Success.Error("") -> help spec
  | Success.Error(mesg) -> usage spec mesg

let parse_argv spec default =
  let argv = Array.sub Sys.argv 1 (pred @@ Array.length @@ Sys.argv) in
  parse argv spec default


(* Long options *)

type 'a _long_option = {
  long_short: char;
  long_option: string;
  long_edit: string -> ('a -> 'a) Success.t;
  long_wants_arg: bool;
} and 'a long_option = (char -> 'a _long_option)

let convert_long c name of_string s =
  try Success.return(of_string s)
  with
  | Invalid_argument(mesg)
  | Failure(mesg) -> error_convert_long c name mesg s
  | _ -> error "Getopts.supervise_convert_long"
(* It is not permitted to of_string to throw something other than a
   failure or an invalid argument. *)

let long_flag name edit c = {
  long_short = c;
  long_option = name;
  long_edit = (function _ -> Success.return edit);
  long_wants_arg = false;
}

let long_option of_string name edit c = {
  long_short = c;
  long_option = name;
  long_edit =
    (fun s -> Success.Infix.(edit <$> convert_long c name of_string s));
  long_wants_arg = true;
}

let long_char x =
  long_option char_of_string x

let long_bool x =
  long_option bool_of_string x

let long_string x =
  long_option (fun x -> x) x

let long_int x =
  long_option int_of_string x

let long_float x =
  long_option float_of_string x

let long_option_get_name s =
  Success.return
    (try String.sub s 0 (String.index s '=')
     with Not_found -> s)

let long_option_get_optarg opt name s =
  try
    let l = String.length s in
    let i = String.index s '=' in
    Success.return(String.sub s (i+1) (l-i-1))
  with Not_found -> error_argument_long opt name

let long_option_lookup opt lst name =
  try Success.return(List.find (fun x -> name = x.long_option) lst)
  with Not_found ->  error_illegal_long opt name

let long_option_edit opt lst s =
  let open Success.Infix in
  long_option_get_name s
  >>= long_option_lookup opt lst
  >>= fun m ->
  if m.long_wants_arg then
    long_option_get_optarg opt m.long_option s >>= m.long_edit
  else
    m.long_edit ""

let long opt lst descr = {
  option = opt;
  edit = long_option_edit opt (List.map ((|>) opt) lst);
  help = descr;
  wants_arg = true;
}


(* Definition of callbacks *)

let store r v =
  r := v

let set v r () =
  r := v

let queue a v () =
  a := !a @ [v]
