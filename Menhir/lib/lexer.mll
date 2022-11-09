{
  open Lexing
  open Parser
  open Positions
  open Errors

  let maybe_read_line () =
  try Some(read_line())
  with End_of_file -> None

let rec loop acc =
  match maybe_read_line () with
  | Some(line) -> loop (line :: acc)
  | None -> List.iter print_endline acc

let () = loop []
  (* Updates the line counter, which is used in some error messages. *)

  let update_loc lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

  (* Extracts a chunk out of the source file. *)

  let chunk ofs1 ofs2 =
    let contents = Errors.get_file_contents() in
    let len = ofs2 - ofs1 in
    String.sub contents ofs1 len

  (* Extracts a chunk out of the source file, delimited by
     one position and extending to the end of the file. *)

  let echunk ofs1 =
    let contents = Errors.get_file_contents() in
    let len = String.length contents - ofs1 in
    String.sub contents ofs1 len

    (* Add whitespace so that the column numbers match those of the source file.
       If requested, add parentheses so that the semantic action can be inserted
       into other code without ambiguity. *)
    let content =
      if parenthesize then
          (String.make (pos1.pos_cnum - pos1.pos_bol - 1) ' ') ^ "(" ^ content ^ ")"
      else
        (String.make (pos1.pos_cnum - pos1.pos_bol) ' ') ^ content
    in
    {
      Stretch.stretch_filename = Errors.get_filename();
      Stretch.stretch_linenum = pos1.pos_lnum;
      Stretch.stretch_linecount = pos2.pos_lnum - pos1.pos_lnum;
      Stretch.stretch_content = content;
      Stretch.stretch_raw_content = raw_content;
      Stretch.stretch_keywords = pkeywords
    }

  (* Translates the family of position-related keywords to abstract
     syntax. *)

  let mk_keyword lexbuf w f n id =
    let where =
      match w with
      | Some _ ->
          Keyword.WhereStart
      | None ->
          Keyword.WhereEnd
    and flavor =
      match f with
      | Some _ ->
          Keyword.FlavorPosition
      | None ->
          Keyword.FlavorOffset
    and subject =
      match n, id with
      | Some n, None ->
          Keyword.RightDollar (int_of_string n)
      | None, Some id ->
          Keyword.RightNamed id
      | None, None ->
          Keyword.Left
      | Some _, Some _ ->
          assert false
    in
    let keyword = Keyword.Position (subject, where, flavor) in
    with_cpos lexbuf keyword

  (* A short-hand. *)

  let error1 pos msg =
    Error.error (Positions.one pos) msg

}

let newline = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ';' ]

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)

let poskeyword =
  '$'
  (("start" as w) | "end")
  (("pos" as f) | "ofs")
  ( '(' ( '$' (['0'-'9']+ as n) | ((lowercase identchar*) as id)) ')')?


rule main = parse
| "%token"
    { TOKEN }
| "%start"
    { START }
| "%%"
    { let ofs = lexeme_end lexbuf in
      PERCENTPERCENT (lazy (echunk ofs)) }
| ":"
    { COLON }
| ","
    { COMMA }
| "="
    { EQUAL }
| "("
    { LPAREN }
| ")"
    { RPAREN }
| "|"
    { BAR }
| "?"
    { QUESTION }
| "*"
    { STAR }
| "+"
    { PLUS }
| newline
    { update_loc lexbuf; main lexbuf }
| whitespace+
    { main lexbuf }
| eof
    { EOF }
| _
    { error1 (lexeme_start_p lexbuf) "unexpected character(s)." }







(*----------------------------------------*)
(* . --  любой символ *)
(* [] -- любой символ из указанных в скобках *)
(* Можно указывать с диапазоном *)
(* $ -- конец строки*)
(* \. -- точка *)
(* ^ -- начало строки *)
(* [^a] -- не символ а *)
(* \d -- любая цифра *)
(* \D -- все, что угодно, кроме цифр *)
(* \s -- пробел *)
(* \S -- все, кроме пробелов *)
(* \w -- буква *)
(* \W -- все, кроме букв *)
(* \b -- граница слова *)
(* Квантификация *)
(* Хотим, например, чтобы символ а повторялся пять раз. Тогда напечатаем a{3} *)
(* * -- от нуля и выше *)
(* + -- от одного и выше *)

(*regex for %token tokenname*)
    (* %token ... *)

