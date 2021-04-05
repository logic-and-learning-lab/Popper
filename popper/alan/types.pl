%% TYPE MATCHING
var_type(Clause,Var,Type):-
    var_in_literal(Clause,P,Vars,Var),
    var_pos(Var,Vars,Pos),
    type(P,Pos,Type).
:-
    clause_var(Clause,Var),
    #count{Type : var_type(Clause,Var,Type)} > 1.