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
:- prop(abcdef_abcdfe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), E>F.
:- prop(abcdef_abcedf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>E.
:- prop(abcdef_abcfed,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>F.
:- prop(abcdef_abdcef,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_abdcfe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_abedcf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>E.
:- prop(abcdef_abefcd,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>E.
:- prop(abcdef_abfdec,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>F.
:- prop(abcdef_abfedc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>E.
:- prop(abcdef_acbdef,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_acbdfe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_acbedf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_acbefd,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_acbfed,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_acdbfe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), E>F.
:- prop(abcdef_acefbd,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>F.
:- prop(abcdef_acfedb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>E.
:- prop(abcdef_adcbef,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_adcbfe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_adebcf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_adebfc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_adefcb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>E.
:- prop(abcdef_adfbec,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_adfebc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>F.
:- prop(abcdef_aecdbf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>E.
:- prop(abcdef_aecfbd,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>E.
:- prop(abcdef_aedcbf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_aedcfb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_aedfbc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>E.
:- prop(abcdef_aefdbc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>E.
:- prop(abcdef_afcdeb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>F.
:- prop(abcdef_afcedb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>E.
:- prop(abcdef_afdceb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_afdecb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>F.
:- prop(abcdef_afedcb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>E.
:- prop(abcdef_bacdef,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_bacdfe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_bacedf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_bacefd,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_bacfed,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_badcef,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_badcfe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_badecf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_badefc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_badfce,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_badfec,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_baedcf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_baedfc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_baefcd,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_baefdc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_bafdec,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_bafedc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>B.
:- prop(abcdef_bcadfe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), E>F.
:- prop(abcdef_bcaedf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>E.
:- prop(abcdef_bcafed,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>F.
:- prop(abcdef_bcdafe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), E>F.
:- prop(abcdef_bcefad,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>F.
:- prop(abcdef_bcfeda,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>E.
:- prop(abcdef_bdacfe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), E>F.
:- prop(abcdef_bdcafe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), E>F.
:- prop(abcdef_bdeacf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>E.
:- prop(abcdef_bdefca,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>E.
:- prop(abcdef_bdfaec,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>F.
:- prop(abcdef_bdfeac,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>F.
:- prop(abcdef_beafcd,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>F.
:- prop(abcdef_becfad,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>F.
:- prop(abcdef_bedcaf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_bedcfa,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_befadc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>F.
:- prop(abcdef_befdac,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>F.
:- prop(abcdef_bfaedc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>E.
:- prop(abcdef_bfceda,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>E.
:- prop(abcdef_bfdcae,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_bfdcea,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_bfeacd,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>E.
:- prop(abcdef_bfedca,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>E.
:- prop(abcdef_cbadef,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cbadfe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cbaedf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cbaefd,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cbafed,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cbdafe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), E>F.
:- prop(abcdef_cbefad,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>F.
:- prop(abcdef_cbfeda,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>E.
:- prop(abcdef_cdabef,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cdabfe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cdaebf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cdaefb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cdafbe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cdafeb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cdbafe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), E>F.
:- prop(abcdef_cdebaf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_cdebfa,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_cdfbae,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_cdfbea,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_ceadbf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_ceadfb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_ceafbd,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_ceafdb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cebfad,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>F.
:- prop(abcdef_cedabf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>E.
:- prop(abcdef_cedfba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>E.
:- prop(abcdef_cefabd,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>E.
:- prop(abcdef_cefdba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>E.
:- prop(abcdef_cfadeb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cfaedb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>C.
:- prop(abcdef_cfbeda,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>E.
:- prop(abcdef_cfdaeb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>F.
:- prop(abcdef_cfdeab,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>F.
:- prop(abcdef_cfeadb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>F.
:- prop(abcdef_cfedab,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>F.
:- prop(abcdef_dbcaef,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_dbcafe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_dbeacf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_dbeafc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_dbefca,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>E.
:- prop(abcdef_dbfaec,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_dbfeac,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>F.
:- prop(abcdef_dcbaef,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_dcbafe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_dcbeaf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_dcbefa,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_dcbfae,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_dcbfea,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_dceabf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_dceafb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_dcfabe,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_dcfaeb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_decabf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_decafb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_decfba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>E.
:- prop(abcdef_defabc,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_defacb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_defbac,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>F.
:- prop(abcdef_defcba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>E.
:- prop(abcdef_dfcaeb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_dfceab,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>F.
:- prop(abcdef_dfeacb,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>D.
:- prop(abcdef_dfebca,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>E.
:- prop(abcdef_dfecab,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>F.
:- prop(abcdef_ebcdaf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>E.
:- prop(abcdef_ebcfad,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>E.
:- prop(abcdef_ebdcaf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_ebdcfa,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_ebdfac,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>E.
:- prop(abcdef_ebfdac,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>E.
:- prop(abcdef_ecbdaf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_ecbdfa,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_ecbfad,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_ecbfda,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_ecdbaf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>E.
:- prop(abcdef_ecdfab,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>E.
:- prop(abcdef_ecfbad,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>E.
:- prop(abcdef_ecfdab,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>E.
:- prop(abcdef_edcbaf,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_edcbfa,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_edcfab,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>E.
:- prop(abcdef_edfbac,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_edfbca,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_edfcab,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>E.
:- prop(abcdef_efcdab,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>E.
:- prop(abcdef_efdcab,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_efdcba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_fbcdea,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>F.
:- prop(abcdef_fbceda,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), D>E.
:- prop(abcdef_fbdcea,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.
:- prop(abcdef_fbdeca,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>F.
:- prop(abcdef_fbedca,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>E.
:- prop(abcdef_fcbdea,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_fcbeda,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>C.
:- prop(abcdef_fcdbea,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>F.
:- prop(abcdef_fcdeba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>F.
:- prop(abcdef_fcebda,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>F.
:- prop(abcdef_fcedba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>F.
:- prop(abcdef_fdcbea,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_fdceba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>F.
:- prop(abcdef_fdebca,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>D.
:- prop(abcdef_fdecba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), A>F.
:- prop(abcdef_fecdba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), B>E.
:- prop(abcdef_fedcba,(P,P)), body_literal(Rule,P,_,(A,B,C,D,E,F)), C>D.