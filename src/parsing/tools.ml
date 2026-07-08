open Lang
open Ast

let mk_curried_fun params body =
  List.fold_right (fun param body ->
    EFunction { param ; body }
  ) params body

let mk_curried_funtype params codomain mode =
  List.fold_right (fun (_, domain) codomain ->
    ETypeFun { domain ; codomain ; mode }
  ) params codomain

let extract_param_names params =
  List.map fst params
