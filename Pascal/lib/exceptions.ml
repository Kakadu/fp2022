(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

exception DupVarName of name
exception VariableNotFound of name
exception FunctionNotFound of name
exception TypeNotFound of name
exception NotAType of name
exception NotAConst of name
exception NonConstCall
exception NotAVariable of name
exception NotAFunction of name
exception NotAStdFunction of name
exception NotIterable of vtype
exception NotInALoop
exception NotInAFunction
exception BinOpTypeError of binop * vtype * vtype
exception UnOpTypeError of unop * vtype
exception StdFunctionTypeError of name * vtype list
exception GlobalFunctionExpected of name
exception RecordTypeError of vtype
exception RecordFieldError of vtype * name
exception ArrayTypeError of vtype
exception ArrayOutOfInd of vtype * value
exception InvalidCall of name
exception CantCall of value
exception InvalidType of vtype * vtype
exception CantCast of value * vtype
exception RunTimeError
exception ParserError
exception SemanticError
exception LeftValError of expr
exception TypeError
exception PascalInterp of exn
