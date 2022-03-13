max_vars(7).
max_body(8).
max_clauses(1).

head_pred(output,3).
body_pred(input,3).
body_pred(succ,2).

body_pred(grey,1).
body_pred(black,1).
%% body_pred(blue,1).
body_pred(cyan,1).
%% body_pred(red,1).
%% body_pred(orange,1).
%% body_pred(yellow,1).

body_pred(igrey,2).
body_pred(iblack,2).
%% body_pred(iblue,2).
%% body_pred(ired,2).
%% body_pred(iorange,2).
%% body_pred(iyellow,2).
body_pred(icyan,2).

type(input,(int, int, colour)).
type(output,(int, int, colour)).
type(succ,(int, int)).
%% type(prec,(int, int)).

type(grey,(colour, )).
type(black,(colour, )).
type(blue,(colour, )).
type(red,(colour, )).
type(orange,(colour, )).
type(yellow,(colour, )).
type(cyan,(colour, )).

type(igrey,(int, int)).
type(iblack,(int, int)).
type(iblue,(int, int)).
type(ired,(int, int)).
type(iorange,(int, int)).
type(iyellow,(int, int)).
type(icyan,(int, int)).


:-
    body_literal(output,_,(A,B,C1)),
    body_literal(output,_,(A,B,C2)),
    C1 != C2.

:-
    body_literal(input,_,(A,B,C1)),
    body_literal(input,_,(A,B,C2)),
    C1 != C2.



colour(igrey).
colour(iblack).
colour(iblue).
colour(ired).
colour(iorange).
colour(iyellow).
colour(icyan).

:-
    colour(P),
    colour(Q),
    P < Q,
    body_literal(P,_,(A,B)),
    body_literal(Q,_,(A,B)).

%% prop(unique_b_a,succ).
%% prop(unique_b_a,igrey).
prop(unique_ab_c,input).
%% prop(unique_a_b,succ).
%% prop(unique_a_b,igrey).
%% prop(unique_a_b,icyan).
prop(asymmetric_abc_bac,input).
prop(asymmetric_ab_ba,succ).
prop(asymmetric_ab_ba,iblack).
prop(asymmetric_ab_ba,icyan).
prop(antitriangular,succ).
prop(antitriangular,igrey).
prop(antitriangular,icyan).
prop(antitransitive,succ).
prop(antitransitive,igrey).
prop(antitransitive,icyan).
prop(singleton,black).
prop(singleton,blue).
prop(singleton,cyan).
prop(singleton,red).
prop(singleton,orange).
prop(singleton,yellow).
prop(singleton,grey).
prop(unsat_pair,red,grey).
prop(unsat_pair,orange,grey).
prop(unsat_pair,yellow,grey).
prop(unsat_pair,grey,black).
prop(unsat_pair,blue,black).
prop(unsat_pair,cyan,black).
prop(unsat_pair,red,black).
prop(unsat_pair,orange,black).
prop(unsat_pair,yellow,black).
prop(unsat_pair,grey,blue).
prop(unsat_pair,cyan,blue).
prop(unsat_pair,red,blue).
prop(unsat_pair,orange,blue).
prop(unsat_pair,yellow,blue).
prop(unsat_pair,grey,cyan).
prop(unsat_pair,red,cyan).
prop(unsat_pair,orange,cyan).
prop(unsat_pair,yellow,cyan).
prop(unsat_pair,yellow,red).
prop(unsat_pair,red,orange).
prop(unsat_pair,yellow,orange).
prop(unsat_pair,succ,igrey).
prop(unsat_pair,igrey,iblack).
prop(unsat_pair,icyan,iblack).
prop(unsat_pair,igrey,icyan).