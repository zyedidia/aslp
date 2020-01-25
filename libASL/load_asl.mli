(****************************************************************
 * Functions for processing ASL files
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
module TC  = Tcheck

val opt_verbose : bool ref

val read_file   : string -> bool -> Asl_ast.declaration list
val read_spec   : string -> Asl_ast.declaration list

val read_impdef : TC.Env.t -> AST.l -> string -> (string * AST.expr)
val read_expr   : TC.Env.t -> AST.l -> string -> AST.expr
val read_stmt   : TC.Env.t -> string -> AST.stmt
