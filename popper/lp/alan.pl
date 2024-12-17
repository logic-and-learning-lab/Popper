%% ##################################################
%% THIS FILE CONTAINS THE ASP PROGRAM GENERATOR, CALLED ALAN
%% ##################################################

#defined direction_/3.
#defined type/2.
#defined non_datalog/0.

#show body_literal/4.

singleton(V):-
    var(V),
    not head_var(_, V),
    #count{P, Vars : body_literal(_, P, _, Vars), var_member(V, Vars)} == 1.

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

{body_literal(0,P,A,Vars)}:-
    body_pred(P,A),
    vars(A,Vars),
    not bad_body(P,Vars),
    not type_mismatch(P,Vars).

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

bad_body(P, Vars):- prop(ab_ba,(P,P)), vars(_, Vars), Vars=(A,B), A>B.
bad_body(P, Vars):- prop(ab_ba,(P,P)), vars(_, Vars), Vars=(A,B), A>B.
bad_body(P, Vars):- prop(abc_acb,(P,P)), vars(_, Vars), Vars=(A,B,C), B>C.
bad_body(P, Vars):- prop(abc_bac,(P,P)), vars(_, Vars), Vars=(A,B,C), A>B.
bad_body(P, Vars):- prop(abc_cba,(P,P)), vars(_, Vars), Vars=(A,B,C), A>C.
bad_body(P, Vars):- prop(abcd_abdc,(P,P)), vars(_, Vars), Vars=(A,B,C,D), C>D.
bad_body(P, Vars):- prop(abcd_acbd,(P,P)), vars(_, Vars), Vars=(A,B,C,D), B>C.
bad_body(P, Vars):- prop(abcd_adcb,(P,P)), vars(_, Vars), Vars=(A,B,C,D), B>D.
bad_body(P, Vars):- prop(abcd_bacd,(P,P)), vars(_, Vars), Vars=(A,B,C,D), A>B.
bad_body(P, Vars):- prop(abcd_badc,(P,P)), vars(_, Vars), Vars=(A,B,C,D), A>B.
bad_body(P, Vars):- prop(abcd_cbad,(P,P)), vars(_, Vars), Vars=(A,B,C,D), A>C.
bad_body(P, Vars):- prop(abcd_cdab,(P,P)), vars(_, Vars), Vars=(A,B,C,D), A>C.
bad_body(P, Vars):- prop(abcd_dbca,(P,P)), vars(_, Vars), Vars=(A,B,C,D), A>D.
bad_body(P, Vars):- prop(abcd_dcba,(P,P)), vars(_, Vars), Vars=(A,B,C,D), B>C.
bad_body(P, Vars):- prop(abcde_abced,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), D>E.
bad_body(P, Vars):- prop(abcde_abdce,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), C>D.
bad_body(P, Vars):- prop(abcde_abedc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), C>E.
bad_body(P, Vars):- prop(abcde_acbde,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), B>C.
bad_body(P, Vars):- prop(abcde_acbed,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), B>C.
bad_body(P, Vars):- prop(abcde_adcbe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), B>D.
bad_body(P, Vars):- prop(abcde_adebc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), B>D.
bad_body(P, Vars):- prop(abcde_aecdb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), B>E.
bad_body(P, Vars):- prop(abcde_aedcb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), C>D.
bad_body(P, Vars):- prop(abcde_bacde,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>B.
bad_body(P, Vars):- prop(abcde_baced,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>B.
bad_body(P, Vars):- prop(abcde_badce,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>B.
bad_body(P, Vars):- prop(abcde_badec,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>B.
bad_body(P, Vars):- prop(abcde_baedc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>B.
bad_body(P, Vars):- prop(abcde_bcaed,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), D>E.
bad_body(P, Vars):- prop(abcde_bdeac,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), C>E.
bad_body(P, Vars):- prop(abcde_bedca,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), C>D.
bad_body(P, Vars):- prop(abcde_cbade,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>C.
bad_body(P, Vars):- prop(abcde_cbaed,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>C.
bad_body(P, Vars):- prop(abcde_cdabe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>C.
bad_body(P, Vars):- prop(abcde_cdaeb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>C.
bad_body(P, Vars):- prop(abcde_cdeba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), B>D.
bad_body(P, Vars):- prop(abcde_ceadb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>C.
bad_body(P, Vars):- prop(abcde_cedab,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), B>E.
bad_body(P, Vars):- prop(abcde_dbcae,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>D.
bad_body(P, Vars):- prop(abcde_dbeac,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>D.
bad_body(P, Vars):- prop(abcde_dcbae,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), B>C.
bad_body(P, Vars):- prop(abcde_dcbea,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), B>C.
bad_body(P, Vars):- prop(abcde_dceab,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>D.
bad_body(P, Vars):- prop(abcde_decab,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>D.
bad_body(P, Vars):- prop(abcde_ebcda,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>E.
bad_body(P, Vars):- prop(abcde_ebdca,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), C>D.
bad_body(P, Vars):- prop(abcde_ecbda,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), B>C.
bad_body(P, Vars):- prop(abcde_ecdba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), A>E.
bad_body(P, Vars):- prop(abcde_edcba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E), B>D.
bad_body(P, Vars):- prop(abcdef_abcdfe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), E>F.
bad_body(P, Vars):- prop(abcdef_abcedf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>E.
bad_body(P, Vars):- prop(abcdef_abcfed,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>F.
bad_body(P, Vars):- prop(abcdef_abdcef,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_abdcfe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_abedcf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>E.
bad_body(P, Vars):- prop(abcdef_abefcd,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>E.
bad_body(P, Vars):- prop(abcdef_abfdec,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>F.
bad_body(P, Vars):- prop(abcdef_abfedc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>E.
bad_body(P, Vars):- prop(abcdef_acbdef,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_acbdfe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_acbedf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_acbefd,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_acbfed,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_acdbfe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), E>F.
bad_body(P, Vars):- prop(abcdef_acefbd,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>F.
bad_body(P, Vars):- prop(abcdef_acfedb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>E.
bad_body(P, Vars):- prop(abcdef_adcbef,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_adcbfe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_adebcf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_adebfc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_adefcb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>E.
bad_body(P, Vars):- prop(abcdef_adfbec,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_adfebc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>F.
bad_body(P, Vars):- prop(abcdef_aecdbf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>E.
bad_body(P, Vars):- prop(abcdef_aecfbd,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>E.
bad_body(P, Vars):- prop(abcdef_aedcbf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_aedcfb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_aedfbc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>E.
bad_body(P, Vars):- prop(abcdef_aefdbc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>E.
bad_body(P, Vars):- prop(abcdef_afcdeb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>F.
bad_body(P, Vars):- prop(abcdef_afcedb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>E.
bad_body(P, Vars):- prop(abcdef_afdceb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_afdecb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>F.
bad_body(P, Vars):- prop(abcdef_afedcb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>E.
bad_body(P, Vars):- prop(abcdef_bacdef,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_bacdfe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_bacedf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_bacefd,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_bacfed,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_badcef,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_badcfe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_badecf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_badefc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_badfce,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_badfec,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_baedcf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_baedfc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_baefcd,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_baefdc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_bafdec,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_bafedc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>B.
bad_body(P, Vars):- prop(abcdef_bcadfe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), E>F.
bad_body(P, Vars):- prop(abcdef_bcaedf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>E.
bad_body(P, Vars):- prop(abcdef_bcafed,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>F.
bad_body(P, Vars):- prop(abcdef_bcdafe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), E>F.
bad_body(P, Vars):- prop(abcdef_bcefad,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>F.
bad_body(P, Vars):- prop(abcdef_bcfeda,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>E.
bad_body(P, Vars):- prop(abcdef_bdacfe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), E>F.
bad_body(P, Vars):- prop(abcdef_bdcafe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), E>F.
bad_body(P, Vars):- prop(abcdef_bdeacf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>E.
bad_body(P, Vars):- prop(abcdef_bdefca,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>E.
bad_body(P, Vars):- prop(abcdef_bdfaec,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>F.
bad_body(P, Vars):- prop(abcdef_bdfeac,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>F.
bad_body(P, Vars):- prop(abcdef_beafcd,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>F.
bad_body(P, Vars):- prop(abcdef_becfad,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>F.
bad_body(P, Vars):- prop(abcdef_bedcaf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_bedcfa,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_befadc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>F.
bad_body(P, Vars):- prop(abcdef_befdac,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>F.
bad_body(P, Vars):- prop(abcdef_bfaedc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>E.
bad_body(P, Vars):- prop(abcdef_bfceda,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>E.
bad_body(P, Vars):- prop(abcdef_bfdcae,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_bfdcea,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_bfeacd,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>E.
bad_body(P, Vars):- prop(abcdef_bfedca,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>E.
bad_body(P, Vars):- prop(abcdef_cbadef,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cbadfe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cbaedf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cbaefd,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cbafed,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cbdafe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), E>F.
bad_body(P, Vars):- prop(abcdef_cbefad,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>F.
bad_body(P, Vars):- prop(abcdef_cbfeda,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>E.
bad_body(P, Vars):- prop(abcdef_cdabef,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cdabfe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cdaebf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cdaefb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cdafbe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cdafeb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cdbafe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), E>F.
bad_body(P, Vars):- prop(abcdef_cdebaf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_cdebfa,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_cdfbae,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_cdfbea,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_ceadbf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_ceadfb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_ceafbd,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_ceafdb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cebfad,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>F.
bad_body(P, Vars):- prop(abcdef_cedabf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>E.
bad_body(P, Vars):- prop(abcdef_cedfba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>E.
bad_body(P, Vars):- prop(abcdef_cefabd,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>E.
bad_body(P, Vars):- prop(abcdef_cefdba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>E.
bad_body(P, Vars):- prop(abcdef_cfadeb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cfaedb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>C.
bad_body(P, Vars):- prop(abcdef_cfbeda,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>E.
bad_body(P, Vars):- prop(abcdef_cfdaeb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>F.
bad_body(P, Vars):- prop(abcdef_cfdeab,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>F.
bad_body(P, Vars):- prop(abcdef_cfeadb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>F.
bad_body(P, Vars):- prop(abcdef_cfedab,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>F.
bad_body(P, Vars):- prop(abcdef_dbcaef,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_dbcafe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_dbeacf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_dbeafc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_dbefca,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>E.
bad_body(P, Vars):- prop(abcdef_dbfaec,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_dbfeac,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>F.
bad_body(P, Vars):- prop(abcdef_dcbaef,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_dcbafe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_dcbeaf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_dcbefa,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_dcbfae,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_dcbfea,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_dceabf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_dceafb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_dcfabe,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_dcfaeb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_decabf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_decafb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_decfba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>E.
bad_body(P, Vars):- prop(abcdef_defabc,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_defacb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_defbac,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>F.
bad_body(P, Vars):- prop(abcdef_defcba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>E.
bad_body(P, Vars):- prop(abcdef_dfcaeb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_dfceab,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>F.
bad_body(P, Vars):- prop(abcdef_dfeacb,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>D.
bad_body(P, Vars):- prop(abcdef_dfebca,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>E.
bad_body(P, Vars):- prop(abcdef_dfecab,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>F.
bad_body(P, Vars):- prop(abcdef_ebcdaf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>E.
bad_body(P, Vars):- prop(abcdef_ebcfad,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>E.
bad_body(P, Vars):- prop(abcdef_ebdcaf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_ebdcfa,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_ebdfac,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>E.
bad_body(P, Vars):- prop(abcdef_ebfdac,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>E.
bad_body(P, Vars):- prop(abcdef_ecbdaf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_ecbdfa,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_ecbfad,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_ecbfda,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_ecdbaf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>E.
bad_body(P, Vars):- prop(abcdef_ecdfab,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>E.
bad_body(P, Vars):- prop(abcdef_ecfbad,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>E.
bad_body(P, Vars):- prop(abcdef_ecfdab,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>E.
bad_body(P, Vars):- prop(abcdef_edcbaf,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_edcbfa,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_edcfab,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>E.
bad_body(P, Vars):- prop(abcdef_edfbac,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_edfbca,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_edfcab,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>E.
bad_body(P, Vars):- prop(abcdef_efcdab,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>E.
bad_body(P, Vars):- prop(abcdef_efdcab,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_efdcba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_fbcdea,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>F.
bad_body(P, Vars):- prop(abcdef_fbceda,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), D>E.
bad_body(P, Vars):- prop(abcdef_fbdcea,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.
bad_body(P, Vars):- prop(abcdef_fbdeca,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>F.
bad_body(P, Vars):- prop(abcdef_fbedca,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>E.
bad_body(P, Vars):- prop(abcdef_fcbdea,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_fcbeda,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>C.
bad_body(P, Vars):- prop(abcdef_fcdbea,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>F.
bad_body(P, Vars):- prop(abcdef_fcdeba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>F.
bad_body(P, Vars):- prop(abcdef_fcebda,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>F.
bad_body(P, Vars):- prop(abcdef_fcedba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>F.
bad_body(P, Vars):- prop(abcdef_fdcbea,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_fdceba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>F.
bad_body(P, Vars):- prop(abcdef_fdebca,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>D.
bad_body(P, Vars):- prop(abcdef_fdecba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), A>F.
bad_body(P, Vars):- prop(abcdef_fecdba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), B>E.
bad_body(P, Vars):- prop(abcdef_fedcba,(P,P)), vars(_, Vars), Vars=(A,B,C,D,E,F), C>D.

%% TRY TO REDUCE THE NUMBER OF RULES WITH REDUNDANT LITERALS

:-
    body_literal(_,P,_,(A,B)),
    body_literal(_,P,_,(A,C)),
    B!=C,
    singleton(B).

:-
    body_literal(_,P,_,(B,A)),
    body_literal(_,P,_,(C,A)),
    B!=C,
    singleton(B).

:-
    body_literal(_,P,_,(A,B,X)),
    body_literal(_,P,_,(A,B,Y)),
    X!=Y,
    singleton(X).

:-
    body_literal(_,P,_,(A,X,B)),
    body_literal(_,P,_,(A,Y,B)),
    X!=Y,
    singleton(X).

:-
    body_literal(_,P,_,(A,B,C)),
    body_literal(_,P,_,(A,X,Y)),
    B!=X,
    C!=Y,
    singleton(X),
    singleton(Y).

:-
    body_literal(_,P,_,(B,A,C)),
    body_literal(_,P,_,(X,A,Y)),
    B!=X,
    C!=Y,
    singleton(X).

:-
    body_literal(_,P,_,(B,C,A)),
    body_literal(_,P,_,(X,Y,A)),
    B!=X,
    C!=Y,
    singleton(X).

:-
    body_literal(_,P,_,(X,V0,V1)),
    body_literal(_,P,_,(Y,V0,V1)),
    X!=Y,
    singleton(X).

:-
    body_literal(_,P,_,(V0,V4,V3,V5)),
    body_literal(_,P,_,(V0,V2,V3,V1)),
    V4!=V2,
    V5!=V1,
    singleton(V2),
    singleton(V1).

:-
    body_literal(_,P,_,(V0,V1,V2,X)),
    body_literal(_,P,_,(V0,V1,V2,Y)),
    X!=Y,
    singleton(Y).

:-
    body_literal(_,P,_,(V0,V3,V4,V1)),
    body_literal(_,P,_,(V0,V2,V4,V1)),
    V3!=V2,
    singleton(V3).

:-
    body_literal(_,P,_,(V0,V4,V3,V1)),
    body_literal(_,P,_,(V0,V4,V2,V1)),
    V3!=V2,
    singleton(V3).

:-
    body_literal(_,P,_,(V0,V2,V3,V1)),
    body_literal(_,P,_,(V0,V5,V4,V1)),
    V2!=V5,
    V3!=V4,
    singleton(V5),
    singleton(V4).

:-
    body_literal(_,P,_,(V0,V3,V5,V2)),
    body_literal(_,P,_,(V0,V3,V4,V1)),
    V5!=V3,
    V2!=V1,
    singleton(V4),
    singleton(V1).

:-
    body_literal(_,P,_,(V0,V4,V3,V2)),
    body_literal(_,P,_,(V0,V5,V6,V1)),
    V4!=V5,
    V3!=V6,
    V2!=V1,
    singleton(V5),
    singleton(V6),
    singleton(V1).