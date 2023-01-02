exception InvalidToken of string * string

val from_string : (Parser.token, 'a) MenhirLib.Convert.traditional -> string -> 'a
