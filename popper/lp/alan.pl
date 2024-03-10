%% ##################################################
%% THIS FILE CONTAINS THE ASP PROGRAM GENERATOR, CALLED ALAN
%% ##################################################

#defined direction_/3.
#defined type/2.
#defined non_datalog/0.

#show body_literal/4.


%% max_size(K):-
%%     custom_max_size(K).
max_size(K):-
    max_body(M),
    max_clauses(N),
    K = (M+1)*N.
size(N):-
    max_size(MaxSize),
    N = 2..MaxSize,
    #sum{K+1,Rule : body_size(Rule,K)} == N.

%% THIS DOES NOT WORK!!!???
%% size(N+1):-
%%     body_size(0,N).
:- not size(_).

%% ********** BASE CASE (RULE 0) **********
head_literal(0,P,A,Vars):-
    head_pred(P,A),
    head_vars(A,Vars).

1 {body_literal(0,P,A,Vars): body_pred(P,A), vars(A,Vars), not type_mismatch(P,Vars)} M :-
    max_body(M).

type_mismatch(P,Vars):-
    var_pos(Var,Vars,Pos),
    type(P,Types),
    pred_arg_type(P,Pos,T1),
    fixed_var_type(Var,T2),
    T1 != T2.

%% THERE IS A CLAUSE IF THERE IS A HEAD LITERAL
clause(C):-
    head_literal(C,_,_,_).

%% NUM BODY LITERALS OF A CLAUSE
%% TODO: IMPROVE AS EXPENSIVE
%% grounding is > c * (n choose k), where n = |Herbrand base| and k = MaxN
body_size(Rule,N):-
    clause(Rule),
    max_body(MaxN),
    N > 0,
    N <= MaxN,
    #count{P,Vars : body_literal(Rule,P,_,Vars)} == N.

%% USE VARS IN ORDER IN A CLAUSE
:-
    clause_var(C,Var1),
    Var1 > 1,
    Var2 = 1..Var1-1,
    not clause_var(C,Var2).

%% POSSIBLE VAR
var(0..N-1):-
    max_vars(N).

%% CLAUSE VAR
clause_var(C,Var):-
    head_var(C,Var).

clause_var(C,Var):-
    body_var(C,Var).

%% HEAD VAR
head_var(C,Var):-
    head_literal(C,_,_,Vars),
    var_member(Var,Vars).

%% BODY VAR
body_var(C,Var):-
    body_literal(C,_,_,Vars),
    var_member(Var,Vars).

%% VAR IN A TUPLE OF VARS
var_member(Var,Vars):-
    var_pos(Var,Vars,_).

%% ##################################################
%% BIAS CONSTRAINTS
%% ##################################################
%% DATALOG
:-
    not non_datalog,
    head_var(Rule,Var),
    not body_var(Rule,Var).

%% if non_datalog is true, all vars are valid
%% constraints used by bk cons
valid_var(Rule,Var):-
    non_datalog,
    Rule=0..MaxRules-1,
    max_clauses(MaxRules),
    var(Var).

%% if datalog, a body only variable is valid
valid_var(Rule,Var):-
    not non_datalog,
    Rule=0..MaxRules-1,
    max_clauses(MaxRules),
    var(Var),
    not head_var(Rule, Var).

%% if datalog, a head var must also appear in the body
valid_var(Rule,Var):-
    not non_datalog,
    head_var(Rule,Var),
    body_var(Rule,Var).

%% MUST BE CONNECTED
head_connected(C,Var):-
    head_var(C,Var).
head_connected(C,Var1):-
    head_literal(C,_,A,_),
    Var1 >= A,
    head_connected(C,Var2),
    body_literal(C,_,_,Vars),
    var_member(Var1,Vars),
    var_member(Var2,Vars),
    Var1 != Var2.
:-
    head_literal(C,_,A,_),
    Var >= A,
    body_var(C,Var),
    not head_connected(C,Var).

fixed_var_type(Var, Type):-
    head_literal(_, P, _A, Vars),
    var_pos(Var, Vars, Pos),
    type(P, Types),
    %% head_vars(A, Vars),
    type_pos(Types, Pos, Type).

pred_arg_type(P, Pos, Type):-
    type(P, Types),
    type_pos(Types, Pos, Type).

var_type(C, Var, Type):-
    body_literal(C,P,_,Vars),
    var_pos(Var,Vars,Pos),
    type(P,Types),
    type_pos(Types, Pos, Type).

var_type(C, Var, Type):-
    head_literal(C,P,_,Vars),
    var_pos(Var,Vars,Pos),
    type(P,Types),
    type_pos(Types, Pos, Type).
:-
    clause_var(C,Var),
    #count{Type : var_type(C,Var,Type)} > 1.

deffo_safe(Rule, Var):-
    head_literal(Rule,P,_,Vars),
    var_pos(Var,Vars,Pos),
    direction_(P,Pos,in),
    Rule = 0.

%% VAR SAFE IF HEAD INPUT VAR
safe_bvar(Rule,Var):-
    head_literal(Rule,P,_,Vars),
    var_pos(Var,Vars,Pos),
    direction_(P,Pos,in).

%% VAR SAFE IF A OUTPUT VAR
safe_bvar(Rule,Var):-
    direction_(_,_,_),
    not deffo_safe(Rule, Var),
    body_literal(Rule,P,_,Vars),
    #count{Pos : direction_(P,Pos,in)} == 0,
    var_member(Var,Vars).

%% VAR SAFE IF ALL INPUT VARS ARE SAFE
safe_bvar(Rule,Var):-
    not deffo_safe(Rule, Var),
    body_literal(Rule, P, _, Vars),
    var_member(Var, Vars),
    #count{Pos : direction_(P,Pos,in)} > 0,
    safe_bvar(Rule,Var2) : var_pos(Var2,Vars,Pos), direction_(P,Pos,in).

:-
    direction_(_,_,_),
    body_var(Rule,Var),
    not safe_bvar(Rule,Var).

%% %% ==========================================================================================
%% %% BK BIAS CONSTRAINTS
%% %% ==========================================================================================
%% IDEAS FROM THE PAPER:
%% Learning logic programs by discovering where not to search. A. Cropper and C. Hocquette. AAAI23.

%% :- prop(ab_ba,(P,P)), body_literal(_,P,_,(A,B)), A>B.
%% :- prop(abc_acb,(P,P)), body_literal(_,P,_,(A,B,C)), B>C.
%% :- prop(abc_bac,(P,P)), body_literal(_,P,_,(A,B,C)), A>B.
%% :- prop(abc_cba,P), body_literal(_,P,_,(A,B,C)), A>B.
%% :- prop(abcd_acbd,P), body_literal(_,P,_,(A,B,C,D)), C>B.
%% :- prop(abcd_adcb,P), body_literal(_,P,_,(A,B,C,D)), D>B.


:- prop(ab_ba,(P,P)), body_literal(Rule,P,_,(A,B)), A>B.
:- prop(abc_acb,(P,P)), body_literal(Rule,P,_,(A,B,C)), B>C.
:- prop(abc_bac,(P,P)), body_literal(Rule,P,_,(A,B,C)), A>B.
:- prop(abc_cba,(P,P)), body_literal(Rule,P,_,(A,B,C)), A>C.
:- prop(abcd_abdc,(P,P)), body_literal(Rule,P,_,(A,B,C,D)), C>D.
:- prop(abcd_acbd,(P,P)), body_literal(Rule,P,_,(A,B,C,D)), B>C.
:- prop(abcd_adcb,(P,P)), body_literal(Rule,P,_,(A,B,C,D)), B>D.
:- prop(abcd_bacd,(P,P)), body_literal(Rule,P,_,(A,B,C,D)), A>B.
:- prop(abcd_badc,(P,P)), body_literal(Rule,P,_,(A,B,C,D)), A>B.
:- prop(abcd_cbad,(P,P)), body_literal(Rule,P,_,(A,B,C,D)), A>C.
:- prop(abcd_cdab,(P,P)), body_literal(Rule,P,_,(A,B,C,D)), A>C.
:- prop(abcd_dbca,(P,P)), body_literal(Rule,P,_,(A,B,C,D)), A>D.
:- prop(abcd_dcba,(P,P)), body_literal(Rule,P,_,(A,B,C,D)), B>C.
:- prop(abcde_abced,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), D>E.
:- prop(abcde_abdce,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), C>D.
:- prop(abcde_abedc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), C>E.
:- prop(abcde_acbde,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), B>C.
:- prop(abcde_acbed,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), B>C.
:- prop(abcde_adcbe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), B>D.
:- prop(abcde_adebc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), B>D.
:- prop(abcde_aecdb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), B>E.
:- prop(abcde_aedcb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), C>D.
:- prop(abcde_bacde,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>B.
:- prop(abcde_baced,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>B.
:- prop(abcde_badce,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>B.
:- prop(abcde_badec,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>B.
:- prop(abcde_baedc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>B.
:- prop(abcde_bcaed,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), D>E.
:- prop(abcde_bdeac,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), C>E.
:- prop(abcde_bedca,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), C>D.
:- prop(abcde_cbade,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>C.
:- prop(abcde_cbaed,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>C.
:- prop(abcde_cdabe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>C.
:- prop(abcde_cdaeb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>C.
:- prop(abcde_cdeba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), B>D.
:- prop(abcde_ceadb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>C.
:- prop(abcde_cedab,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), B>E.
:- prop(abcde_dbcae,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>D.
:- prop(abcde_dbeac,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>D.
:- prop(abcde_dcbae,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), B>C.
:- prop(abcde_dcbea,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), B>C.
:- prop(abcde_dceab,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>D.
:- prop(abcde_decab,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>D.
:- prop(abcde_ebcda,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>E.
:- prop(abcde_ebdca,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), C>D.
:- prop(abcde_ecbda,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), B>C.
:- prop(abcde_ecdba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), A>E.
:- prop(abcde_edcba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E)), B>D.
%% abcd_adcb
%% abcd_badc
%% abcd_bcda
%% abcd_cbad
%% abcd_cdab
%% abcd_dabc
%% abcd_dcba

%% prop(abcde_acbed,(input_move,input_move)).
%% prop(abcde_acdeb,(input_move,input_move)).
%% prop(abcde_adcbe,(input_move,input_move)).
%% prop(abcde_adebc,(input_move,input_move)).
%% prop(abcde_aebcd,(input_move,input_move)).
%% prop(abcde_aedcb,(input_move,input_move)).
%% prop(abcd_badc,(distinctcell,distinctcell)).
%% prop(abcd_bcda,(distinctcell,distinctcell)).
%% prop(abcd_cbad,(blackpawnmove,blackpawnmove)).


