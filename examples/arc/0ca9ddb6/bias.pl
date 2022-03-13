max_vars(6).
max_body(6).
max_clauses(1).

head_pred(output,3).
body_pred(input,3).
body_pred(succ,2).

body_pred(black,1).
body_pred(blue,1).
body_pred(red,1).
body_pred(orange,1).
body_pred(yellow,1).

body_pred(iblack,2).
body_pred(iblue,2).
body_pred(ired,2).
body_pred(iorange,2).
body_pred(iyellow,2).

type(input,(int, int, colour)).
type(output,(int, int, colour)).
type(succ,(int, int)).

type(black,(colour, )).
type(blue,(colour, )).
type(red,(colour, )).
type(orange,(colour, )).
type(yellow,(colour, )).

type(iblack,(int, int)).
type(iblue,(int, int)).
type(ired,(int, int)).
type(iorange,(int, int)).
type(iyellow,(int, int)).

colour(iblack).
colour(iblue).
colour(ired).
colour(iorange).
colour(iyellow).
colour(icyan).

:-
    body_literal(output,_,(A,B,C1)),
    body_literal(output,_,(A,B,C2)),
    C1 < C2.

:-
    body_literal(input,_,(A,B,C1)),
    body_literal(input,_,(A,B,C2)),
    C1 < C2.

:-
    colour(P),
    colour(Q),
    P < Q,
    body_literal(P,_,(A,B)),
    body_literal(Q,_,(A,B)).

%% prop(unique_b_a,succ).
prop(unique_b_a,iblue).
prop(unique_b_a,ired).
prop(unique_b_a,iorange).
prop(unique_b_a,iyellow).
prop(unique_ab_c,input).
%% prop(unique_a_b,succ).
prop(unique_a_b,iblue).
prop(unique_a_b,ired).
prop(unique_a_b,iorange).
prop(unique_a_b,iyellow).
prop(asymmetric_abc_bac,input).
prop(asymmetric_ab_ba,succ).
prop(asymmetric_ab_ba,iblack).
prop(asymmetric_ab_ba,iblue).
prop(asymmetric_ab_ba,ired).
prop(antitriangular,succ).
prop(antitriangular,iblue).
prop(antitriangular,ired).
prop(antitriangular,iorange).
prop(antitriangular,iyellow).
prop(antitransitive,succ).
prop(antitransitive,iblue).
prop(antitransitive,ired).
prop(antitransitive,iorange).
prop(antitransitive,iyellow).
prop(singleton,black).
prop(singleton,blue).
prop(singleton,red).
prop(singleton,orange).
prop(singleton,yellow).
prop(singleton,iblue).
prop(singleton,ired).
prop(unsat_pair,blue,black).
prop(unsat_pair,red,black).
prop(unsat_pair,orange,black).
prop(unsat_pair,yellow,black).
prop(unsat_pair,red,blue).
prop(unsat_pair,orange,blue).
prop(unsat_pair,yellow,blue).
prop(unsat_pair,yellow,red).
prop(unsat_pair,red,orange).
prop(unsat_pair,yellow,orange).
prop(unsat_pair,iblue,iblack).
prop(unsat_pair,ired,iblack).
prop(unsat_pair,iorange,iblack).
prop(unsat_pair,iyellow,iblack).
prop(unsat_pair,succ,iblue).
prop(unsat_pair,ired,iblue).
prop(unsat_pair,iorange,iblue).
prop(unsat_pair,iyellow,iblue).
prop(unsat_pair,succ,ired).
prop(unsat_pair,iyellow,ired).
prop(unsat_pair,succ,iorange).
prop(unsat_pair,ired,iorange).
prop(unsat_pair,iyellow,iorange).
prop(unsat_pair,succ,iyellow).