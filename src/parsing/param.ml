open Lang

module type S = sig
  val make_refinement : Ident.t -> tau:Ast.t -> predicate:Ast.t -> Ast.pos_span -> Ast.t
end

module Standard : S = struct
  let make_refinement var ~tau ~predicate _pos =
    Ast.ETypeRefine { var ; tau ; predicate }
end

module Make_ignore_refine () = struct
  let refine_positions : Ast.pos_span list ref = ref []

  let make_refinement _var ~tau ~predicate:_ pos =
    refine_positions := pos :: !refine_positions;
    tau

  let positions () = List.rev !refine_positions
end
