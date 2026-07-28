open Lang

module type S = sig
  val make_refinement : Ident.t -> typ:Ast.t -> pred:Ast.t -> Ast.pos_span -> Ast.t
end

module Standard : S = struct
  let make_refinement var ~typ ~pred _pos =
    Ast.ETypeRefine { var ; typ ; pred }
end

module Make_ignore_refine () = struct
  let refine_positions : Ast.pos_span list ref = ref []

  let make_refinement _var ~typ ~pred:_ pos =
    refine_positions := pos :: !refine_positions;
    typ

  let positions () = List.rev !refine_positions
end
