#show prop/2.
#show prop/3.
#defined holds/2.


same_type(P,Q):- body_pred(P,A), body_pred(Q,A), P > Q, not type(P,_), not type(Q,_).
same_type(P,Q):- body_pred(P,A), body_pred(Q,A), P > Q, type(P,Types), type(Q,Types).
single_type(P):- body_pred(P,_), not type(P,_).
single_type(P):- type(P,(T,T)).
same_arity(P,Q):- body_pred(P,A), body_pred(Q,A).

prop(singleton,P):- body_pred(P,_), #count{Vars : holds(P,Vars)} == 1.
prop(unsat_pair,P,Q):- body_pred(Q,A), P > Q, same_type(P,Q), #count{Vars : holds(P,Vars), holds(Q,Vars)} == 0.

prop(antitransitive,P):- antitransitive_type_check(P), body_pred(P,2), not antitransitive_aux(P).
antitransitive_aux(P):- antitransitive_type_check(P), holds(P,(A,B)), holds(P,(B,C)), holds(P,(A,C)).
antitransitive_type_check(P):- body_pred(P,_), not type(P,_).
antitransitive_type_check(P):- type(P,(TA,TB)), type(P,(TB,TC)), type(P,(TA,TC)).

prop(antitriangular,P):- antitriangular_type_check(P), body_pred(P,2), not antitriangular_aux(P).
antitriangular_aux(P):- antitriangular_type_check(P), holds(P,(A,B)), holds(P,(B,C)), holds(P,(C,A)).
antitriangular_type_check(P):- body_pred(P,_), not type(P,_).
antitriangular_type_check(P):- type(P,(TA,TB)), type(P,(TB,TC)), type(P,(TC,TA)).

prop(asymmetric_ab_ba,P):- not type(P,_), holds(P,(A,B)), not holds(P,(B,A)).
prop(asymmetric_ab_ba,P):- type(P,(Ta,Tb)), type(P,(Tb,Ta)), holds(P,(A,B)), not holds(P,(B,A)).
prop(asymmetric_abc_acb,P):- not type(P,_), holds(P,(A,B,C)), not holds(P,(A,C,B)).
prop(asymmetric_abc_acb,P):- type(P,(Ta,Tb,Tc)), type(P,(Ta,Tc,Tb)), holds(P,(A,B,C)), not holds(P,(A,C,B)).
prop(asymmetric_abc_bac,P):- not type(P,_), holds(P,(A,B,C)), not holds(P,(B,A,C)).
prop(asymmetric_abc_bac,P):- type(P,(Ta,Tb,Tc)), type(P,(Tb,Ta,Tc)), holds(P,(A,B,C)), not holds(P,(B,A,C)).
prop(asymmetric_abc_bca,P):- not type(P,_), holds(P,(A,B,C)), not holds(P,(B,C,A)).
prop(asymmetric_abc_bca,P):- type(P,(Ta,Tb,Tc)), type(P,(Tb,Tc,Ta)), holds(P,(A,B,C)), not holds(P,(B,C,A)).
prop(asymmetric_abc_cab,P):- not type(P,_), holds(P,(A,B,C)), not holds(P,(C,A,B)).
prop(asymmetric_abc_cab,P):- type(P,(Ta,Tb,Tc)), type(P,(Tc,Ta,Tb)), holds(P,(A,B,C)), not holds(P,(C,A,B)).
prop(asymmetric_abc_cba,P):- not type(P,_), holds(P,(A,B,C)), not holds(P,(C,B,A)).
prop(asymmetric_abc_cba,P):- type(P,(Ta,Tb,Tc)), type(P,(Tc,Tb,Ta)), holds(P,(A,B,C)), not holds(P,(C,B,A)).
prop(asymmetric_abcd_abdc,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(A,B,D,C)).
prop(asymmetric_abcd_abdc,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Ta,Tb,Td,Tc)), holds(P,(A,B,C,D)), not holds(P,(A,B,D,C)).
prop(asymmetric_abcd_acbd,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(A,C,B,D)).
prop(asymmetric_abcd_acbd,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Ta,Tc,Tb,Td)), holds(P,(A,B,C,D)), not holds(P,(A,C,B,D)).
prop(asymmetric_abcd_acdb,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(A,C,D,B)).
prop(asymmetric_abcd_acdb,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Ta,Tc,Td,Tb)), holds(P,(A,B,C,D)), not holds(P,(A,C,D,B)).
prop(asymmetric_abcd_adbc,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(A,D,B,C)).
prop(asymmetric_abcd_adbc,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Ta,Td,Tb,Tc)), holds(P,(A,B,C,D)), not holds(P,(A,D,B,C)).
prop(asymmetric_abcd_adcb,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(A,D,C,B)).
prop(asymmetric_abcd_adcb,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Ta,Td,Tc,Tb)), holds(P,(A,B,C,D)), not holds(P,(A,D,C,B)).
prop(asymmetric_abcd_bacd,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(B,A,C,D)).
prop(asymmetric_abcd_bacd,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Tb,Ta,Tc,Td)), holds(P,(A,B,C,D)), not holds(P,(B,A,C,D)).
prop(asymmetric_abcd_badc,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(B,A,D,C)).
prop(asymmetric_abcd_badc,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Tb,Ta,Td,Tc)), holds(P,(A,B,C,D)), not holds(P,(B,A,D,C)).
prop(asymmetric_abcd_bcad,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(B,C,A,D)).
prop(asymmetric_abcd_bcad,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Tb,Tc,Ta,Td)), holds(P,(A,B,C,D)), not holds(P,(B,C,A,D)).
prop(asymmetric_abcd_bcda,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(B,C,D,A)).
prop(asymmetric_abcd_bcda,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Tb,Tc,Td,Ta)), holds(P,(A,B,C,D)), not holds(P,(B,C,D,A)).
prop(asymmetric_abcd_bdac,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(B,D,A,C)).
prop(asymmetric_abcd_bdac,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Tb,Td,Ta,Tc)), holds(P,(A,B,C,D)), not holds(P,(B,D,A,C)).
prop(asymmetric_abcd_bdca,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(B,D,C,A)).
prop(asymmetric_abcd_bdca,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Tb,Td,Tc,Ta)), holds(P,(A,B,C,D)), not holds(P,(B,D,C,A)).
prop(asymmetric_abcd_cabd,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(C,A,B,D)).
prop(asymmetric_abcd_cabd,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Tc,Ta,Tb,Td)), holds(P,(A,B,C,D)), not holds(P,(C,A,B,D)).
prop(asymmetric_abcd_cadb,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(C,A,D,B)).
prop(asymmetric_abcd_cadb,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Tc,Ta,Td,Tb)), holds(P,(A,B,C,D)), not holds(P,(C,A,D,B)).
prop(asymmetric_abcd_cbad,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(C,B,A,D)).
prop(asymmetric_abcd_cbad,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Tc,Tb,Ta,Td)), holds(P,(A,B,C,D)), not holds(P,(C,B,A,D)).
prop(asymmetric_abcd_cbda,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(C,B,D,A)).
prop(asymmetric_abcd_cbda,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Tc,Tb,Td,Ta)), holds(P,(A,B,C,D)), not holds(P,(C,B,D,A)).
prop(asymmetric_abcd_cdab,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(C,D,A,B)).
prop(asymmetric_abcd_cdab,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Tc,Td,Ta,Tb)), holds(P,(A,B,C,D)), not holds(P,(C,D,A,B)).
prop(asymmetric_abcd_cdba,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(C,D,B,A)).
prop(asymmetric_abcd_cdba,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Tc,Td,Tb,Ta)), holds(P,(A,B,C,D)), not holds(P,(C,D,B,A)).
prop(asymmetric_abcd_dabc,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(D,A,B,C)).
prop(asymmetric_abcd_dabc,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Td,Ta,Tb,Tc)), holds(P,(A,B,C,D)), not holds(P,(D,A,B,C)).
prop(asymmetric_abcd_dacb,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(D,A,C,B)).
prop(asymmetric_abcd_dacb,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Td,Ta,Tc,Tb)), holds(P,(A,B,C,D)), not holds(P,(D,A,C,B)).
prop(asymmetric_abcd_dbac,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(D,B,A,C)).
prop(asymmetric_abcd_dbac,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Td,Tb,Ta,Tc)), holds(P,(A,B,C,D)), not holds(P,(D,B,A,C)).
prop(asymmetric_abcd_dbca,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(D,B,C,A)).
prop(asymmetric_abcd_dbca,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Td,Tb,Tc,Ta)), holds(P,(A,B,C,D)), not holds(P,(D,B,C,A)).
prop(asymmetric_abcd_dcab,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(D,C,A,B)).
prop(asymmetric_abcd_dcab,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Td,Tc,Ta,Tb)), holds(P,(A,B,C,D)), not holds(P,(D,C,A,B)).
prop(asymmetric_abcd_dcba,P):- not type(P,_), holds(P,(A,B,C,D)), not holds(P,(D,C,B,A)).
prop(asymmetric_abcd_dcba,P):- type(P,(Ta,Tb,Tc,Td)), type(P,(Td,Tc,Tb,Ta)), holds(P,(A,B,C,D)), not holds(P,(D,C,B,A)).
prop(unique_a_b,P):- body_pred(P,2), not unique_a_b_(P).
prop(unique_a_bc,P):- body_pred(P,3), not unique_a_bc_(P).
prop(unique_a_bcd,P):- body_pred(P,4), not unique_a_bcd_(P).
prop(unique_ab_c,P):- body_pred(P,3), not unique_ab_c_(P).
prop(unique_ab_cd,P):- body_pred(P,4), not unique_ab_cd_(P).
prop(unique_abc_d,P):- body_pred(P,4), not unique_abc_d_(P).
prop(unique_abd_c,P):- body_pred(P,4), not unique_abd_c_(P).
prop(unique_ac_b,P):- body_pred(P,3), not unique_ac_b_(P).
prop(unique_ac_bd,P):- body_pred(P,4), not unique_ac_bd_(P).
prop(unique_acd_b,P):- body_pred(P,4), not unique_acd_b_(P).
prop(unique_ad_bc,P):- body_pred(P,4), not unique_ad_bc_(P).
prop(unique_b_a,P):- body_pred(P,2), not unique_b_a_(P).
prop(unique_b_ac,P):- body_pred(P,3), not unique_b_ac_(P).
prop(unique_b_acd,P):- body_pred(P,4), not unique_b_acd_(P).
prop(unique_bc_a,P):- body_pred(P,3), not unique_bc_a_(P).
prop(unique_bc_ad,P):- body_pred(P,4), not unique_bc_ad_(P).
prop(unique_bcd_a,P):- body_pred(P,4), not unique_bcd_a_(P).
prop(unique_bd_ac,P):- body_pred(P,4), not unique_bd_ac_(P).
prop(unique_c_ab,P):- body_pred(P,3), not unique_c_ab_(P).
prop(unique_c_abd,P):- body_pred(P,4), not unique_c_abd_(P).
prop(unique_cd_ab,P):- body_pred(P,4), not unique_cd_ab_(P).
prop(unique_d_abc,P):- body_pred(P,4), not unique_d_abc_(P).

unique_a_b_(P):-holds(P,(A,_)), #count{B : holds(P,(A,B))} > 1.
unique_a_bc_(P):-holds(P,(A,_,_)), #count{B,C : holds(P,(A,B,C))} > 1.
unique_a_bcd_(P):-holds(P,(A,_,_,_)), #count{B,C,D : holds(P,(A,B,C,D))} > 1.
unique_ab_c_(P):-holds(P,(A,B,_)), #count{C : holds(P,(A,B,C))} > 1.
unique_ab_cd_(P):-holds(P,(A,B,_,_)), #count{C,D : holds(P,(A,B,C,D))} > 1.
unique_abc_d_(P):-holds(P,(A,B,C,_)), #count{D : holds(P,(A,B,C,D))} > 1.
unique_abd_c_(P):-holds(P,(A,B,_,D)), #count{C : holds(P,(A,B,C,D))} > 1.
unique_ac_b_(P):-holds(P,(A,_,C)), #count{B : holds(P,(A,B,C))} > 1.
unique_ac_bd_(P):-holds(P,(A,_,C,_)), #count{B,D : holds(P,(A,B,C,D))} > 1.
unique_acd_b_(P):-holds(P,(A,_,C,D)), #count{B : holds(P,(A,B,C,D))} > 1.
unique_ad_bc_(P):-holds(P,(A,_,_,D)), #count{B,C : holds(P,(A,B,C,D))} > 1.
unique_b_a_(P):-holds(P,(_,B)), #count{A : holds(P,(A,B))} > 1.
unique_b_ac_(P):-holds(P,(_,B,_)), #count{A,C : holds(P,(A,B,C))} > 1.
unique_b_acd_(P):-holds(P,(_,B,_,_)), #count{A,C,D : holds(P,(A,B,C,D))} > 1.
unique_bc_a_(P):-holds(P,(_,B,C)), #count{A : holds(P,(A,B,C))} > 1.
unique_bc_ad_(P):-holds(P,(_,B,C,_)), #count{A,D : holds(P,(A,B,C,D))} > 1.
unique_bcd_a_(P):-holds(P,(_,B,C,D)), #count{A : holds(P,(A,B,C,D))} > 1.
unique_bd_ac_(P):-holds(P,(_,B,_,D)), #count{A,C : holds(P,(A,B,C,D))} > 1.
unique_c_ab_(P):-holds(P,(_,_,C)), #count{A,B : holds(P,(A,B,C))} > 1.
unique_c_abd_(P):-holds(P,(_,_,C,_)), #count{A,B,D : holds(P,(A,B,C,D))} > 1.
unique_cd_ab_(P):-holds(P,(_,_,C,D)), #count{A,B : holds(P,(A,B,C,D))} > 1.
unique_d_abc_(P):-holds(P,(_,_,_,D)), #count{A,B,C : holds(P,(A,B,C,D))} > 1.


prop(countk,P,K):-
    K > 0,
    K < 4,
    body_pred(P,_),
    #count{Vars : holds(P,Vars)} == K.

%% porper(count2,P):-
%%     body_pred(P,_),
%%     #count{Vars : holds(P,Vars} == 2.
%% porper(count3,P):-
%%     body_pred(P,_),
%%     #count{Vars : holds(P,Vars} == 3.
%% porper(count4,P):-
%%     body_pred(P,_),
%%     #count{Vars : holds(P,Vars} == 4.

%% prop(count_a_b,P,N):-
%%     body_pred(P,2),
%%     #max{K : count_a_b(P,K)} == N.


%% prop(symmetric_ab_ba,P):- not type(P,_), holds(P,(A,B)), not holds(P,(B,A)).


prop(symmetric_ab,P):-
    type(P,(Ta,Tb)),
    type(P,(Tb,Ta)),
    body_pred(P,_),
    not symmetric_ab_sat(P).
symmetric_ab_sat(P):-
    holds(P,(A,B)),
    not holds(P,(B,A)).


prop(symmetric_xab,P):-
    type(P,(Ta,Tb,Tc)),
    type(P,(Ta,Tc,Tb)),
    body_pred(P,_),
    not symmetric_xab_aux(P).
symmetric_xab_aux(P):-
    type(P,(Ta,Tb,Tc)),
    type(P,(Ta,Tc,Tb)),
    body_pred(P,_),
    holds(P,(A,B,C)),
    not holds(P,(A,C,B)).



prop(chain,(P,Q)):-
    type(P,(_,Ta)),
    type(Q,(Ta,_)),
    body_pred(P,_),
    body_pred(Q,_),
    not chain_sat((P,Q)).

chain_sat((P,Q)):-
    body_pred(P,_),
    body_pred(Q,_),
    type(P,(_,Ta)),
    type(Q,(Ta,_)),
    holds(P,(_,A)),
    holds(Q,(A,_)).

prop(precon,(P,Q)):-
    type(P,(Ta,)),
    type(Q,(Ta,_,)),
    body_pred(P,1),
    body_pred(Q,2),
    #count{A : holds(P,(A,)), holds(Q,(A,_))} == 0.

prop(postcon,(P,Q)):-
    type(P,(Ta,Tb)),
    type(Q,(Tb,)),
    body_pred(P,2),
    body_pred(Q,1),
    #count{B : holds(P,(_,B)), holds(Q,(B,))} == 0.

prop(pre_postcon,(P,Q,R)):-
    type(P,(Ta,)),
    type(Q,(Ta,Tb)),
    type(R,(Tb,)),
    body_pred(P,_),
    body_pred(Q,_),
    body_pred(R,_),
    not pre_postcon_sat((P,Q,R)).
pre_postcon_sat((P,Q,R)):-
    type(P,(Ta,)),
    type(Q,(Ta,Tb)),
    type(R,(Tb,)),
    body_pred(P,_),
    body_pred(Q,_),
    body_pred(R,_),
    holds(P,(A,)),
    holds(Q,(A,B)),
    holds(R,(B,)).

prop(pab_qac,(P,Q)):-
    type(P,(Ta,_)),
    type(Q,(Ta,_)),
    body_pred(P,_),
    body_pred(Q,_),
    P < Q,
    not pab_qac_sat((P,Q)).
pab_qac_sat((P,Q)):-
    P < Q,
    body_pred(P,_),
    body_pred(Q,_),
    type(P,(Ta,_)),
    type(Q,(Ta,_)),
    holds(P,(A,_)),
    holds(Q,(A,_)).

prop(pab_qca,(P,Q)):-
    type(P,(Ta,_)),
    type(Q,(_,Ta)),
    body_pred(P,_),
    body_pred(Q,_),
    P < Q,
    not pab_qca_sat((P,Q)).
pab_qca_sat((P,Q)):-
    P < Q,
    body_pred(P,_),
    body_pred(Q,_),
    type(P,(Ta,_)),
    type(Q,(_,Ta)),
    holds(P,(A,_)),
    holds(Q,(_,A)).



%% prop(same_pab,P):-
%%     type(P,(Ta,Tb)),
%%     type(P,(Tb,Ta)),
%%     body_pred(P,_),
%%     not symmetric_ab_sat(P).
%% symmetric_ab_sat(P):-
%%     holds(P,(A,B)),
%%     not holds(P,(B,A)).