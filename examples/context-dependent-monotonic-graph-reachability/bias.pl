enable_recursion.

head_pred(path,3).
body_pred(edge,3).
body_pred(gt,2).

type(path,(cd_id,vertex,vertex)).
type(edge,(cd_id,vertex,vertex)).
type(gt,(vertex,vertex)).

% rule to determine variables binding to context-dependent identifiers
cd_id_var_in_body(C,V) :- body_literal(C,_,_,Vars),var_pos(V,Vars,_),var_type(C,V,cd_id).
% prune programs which have bodies which bind context-dependent identifiers that could be different from the head context-dependent identifier
:- head_literal(C,_,_,Vars),var_pos(A,Vars,_),var_type(C,A,cd_id),cd_id_var_in_body(C,B),A!=B.

% silence superfluous warnings
#defined head_literal/4.
#defined var_pos/3.
