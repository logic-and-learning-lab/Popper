
max_body(5).
max_clauses(3).
max_vars(5).

enable_recursion.
head_pred(filter_negative,2).



%% body_pred(element,2). % CANNOT USE IN EVERYTHING BECAUSE OF MEMBER PROBLEM
%% body_pred(increment,2). % METAGOL CRASHES WHEN GIVEN
body_pred(cons,3).
%% body_pred(decrement,2).
body_pred(empty,1).
%% body_pred(even,1).
%% body_pred(geq,2).
body_pred(head,2).
body_pred(negative,1).
%% body_pred(odd,1).
%% body_pred(one,1).
body_pred(positive,1).
body_pred(tail,2).
%% body_pred(zero,1).

%% size 14

%     filter_negative(A,E) :-  empty(A), empty(E)
%     filter_negative(A,D) :-  head(A,B), negative(B), tail(A,C), filter_negative(C,D)
%     filter_negative(A,E) :-  head(A,B), positive(B), tail(A,C), filter_negative(C,D), cons(B,D,E)

direction(cons,(in,in,out)).
%% direction(decrement,(in,out)).
%% direction(element,(in,out)).
direction(empty,(out,)).
%% direction(even,(in,)).
direction(filter_negative,(in,out)).
%% direction(geq,(in,in)).
direction(head,(in,out)).
%% direction(increment,(in,out)).
%% direction(mult,(in,in,out)).
direction(negative,(in,)).
%% direction(odd,(in,)).
%% direction(one,(in,)).
direction(positive,(in,)).
%% direction(sum,(in,in,out)).
direction(tail,(in,out)).
%% direction(zero,(out,)).


:-
    Rule > 0,
    head_pred(P,_),
    body_literal(Rule,empty,_,(A,)),
    body_literal(Rule,P,_,(A,_)).

:-
    Rule > 0,
    head_literal(Rule,P,_,(A,_)),
    body_literal(Rule,P,_,(_,A)).

type(cons,(element,list,list)).
%% type(cons,(list,element,list)).
type(decrement,(element,element)).
type(element,(list,element)).
type(empty,(list,)).
type(even,(element,)).
type(filter_negative,(list, list)).
type(geq,(element,element)).
type(head,(list,element)).
type(increment,(element,element)).
type(mult,(element,element,element)).
type(negative,(element,)).
type(odd,(element,)).
type(one,(element,)).
type(positive,(element,)).
type(sum,(element,element,element)).
type(tail,(list,list)).
type(zero,(element,)).





prop(antitransitive,decrement).
prop(antitransitive,decrement).
prop(antitransitive,increment).
prop(antitransitive,increment).
prop(antitransitive,tail).
prop(antitransitive,tail).
prop(antitriangular,decrement).
prop(antitriangular,decrement).
prop(antitriangular,increment).
prop(antitriangular,increment).
prop(antitriangular,tail).
prop(antitriangular,tail).
prop(asymmetric_ab_ba,decrement).
prop(asymmetric_ab_ba,decrement).
prop(asymmetric_ab_ba,geq).
prop(asymmetric_ab_ba,geq).
prop(asymmetric_ab_ba,increment).
prop(asymmetric_ab_ba,increment).
prop(asymmetric_ab_ba,tail).
prop(asymmetric_ab_ba,tail).
prop(asymmetric_abc_acb,cons).
prop(asymmetric_abc_acb,cons).
%% prop(countk,empty,1).
%% prop(countk,empty_out,1).
%% prop(countk,even,3).
%% prop(countk,odd,2).
%% prop(countk,one,1).
%% prop(countk,zero,1).
prop(postcon,(element,zero)).
prop(postcon,(head,zero)).
prop(postcon,(increment,zero)).
prop(pre_postcon,(empty,element,even)).
prop(pre_postcon,(empty,element,odd)).
prop(pre_postcon,(empty,element,one)).
prop(pre_postcon,(empty,element,zero)).
prop(pre_postcon,(empty,head,even)).
prop(pre_postcon,(empty,head,odd)).
prop(pre_postcon,(empty,head,one)).
prop(pre_postcon,(empty,head,zero)).
prop(pre_postcon,(empty,tail,empty)).
prop(pre_postcon,(empty,tail,empty_out)).
prop(pre_postcon,(empty_out,element,even)).
prop(pre_postcon,(empty_out,element,odd)).
prop(pre_postcon,(empty_out,element,one)).
prop(pre_postcon,(empty_out,element,zero)).
prop(pre_postcon,(empty_out,head,even)).
prop(pre_postcon,(empty_out,head,odd)).
prop(pre_postcon,(empty_out,head,one)).
prop(pre_postcon,(empty_out,head,zero)).
prop(pre_postcon,(empty_out,tail,empty)).
prop(pre_postcon,(empty_out,tail,empty_out)).
prop(pre_postcon,(even,decrement,even)).
prop(pre_postcon,(even,decrement,zero)).
prop(pre_postcon,(even,increment,even)).
prop(pre_postcon,(even,increment,zero)).
prop(pre_postcon,(odd,decrement,odd)).
prop(pre_postcon,(odd,decrement,one)).
prop(pre_postcon,(odd,increment,odd)).
prop(pre_postcon,(odd,increment,one)).
prop(pre_postcon,(odd,increment,zero)).
prop(pre_postcon,(one,decrement,odd)).
prop(pre_postcon,(one,decrement,one)).
prop(pre_postcon,(one,increment,odd)).
prop(pre_postcon,(one,increment,one)).
prop(pre_postcon,(one,increment,zero)).
prop(pre_postcon,(zero,decrement,even)).
prop(pre_postcon,(zero,decrement,odd)).
prop(pre_postcon,(zero,decrement,one)).
prop(pre_postcon,(zero,decrement,zero)).
prop(pre_postcon,(zero,geq,odd)).
prop(pre_postcon,(zero,geq,one)).
prop(pre_postcon,(zero,increment,even)).
prop(pre_postcon,(zero,increment,zero)).
prop(precon,(empty,element)).
prop(precon,(empty,head)).
prop(precon,(empty,tail)).
prop(precon,(empty_out,element)).
prop(precon,(empty_out,head)).
prop(precon,(empty_out,tail)).
prop(precon,(zero,decrement)).
prop(unique_a_b,decrement).
prop(unique_a_b,decrement).
prop(unique_a_b,head).
prop(unique_a_b,head).
prop(unique_a_b,increment).
prop(unique_a_b,increment).
prop(unique_a_b,tail).
prop(unique_a_b,tail).
prop(unique_ab_c,cons).
prop(unique_ab_c,cons).
prop(unique_ac_b,cons).
prop(unique_ac_b,cons).
prop(unique_b_a,decrement).
prop(unique_b_a,decrement).
prop(unique_b_a,increment).
prop(unique_b_a,increment).
prop(unique_bc_a,cons).
prop(unique_bc_a,cons).
prop(unique_c_ab,cons).
prop(unique_c_ab,cons).
prop(unsat_pair,increment,decrement).
prop(unsat_pair,increment,decrement).
prop(unsat_pair,increment,geq).
prop(unsat_pair,increment,geq).
prop(unsat_pair,odd,even).
prop(unsat_pair,odd,even).
prop(unsat_pair,one,even).
prop(unsat_pair,one,even).
prop(unsat_pair,zero,odd).
prop(unsat_pair,zero,odd).
prop(unsat_pair,zero,one).
prop(unsat_pair,zero,one).