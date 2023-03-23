#show prop/2.
#show prop/3.
%% #defined holds/2.

%% prop(antitransitive,P):- antitransitive_type_check(P), body_pred(P,2), not antitransitive_aux(P).
%% antitransitive_aux(P):- antitransitive_type_check(P), holds(P,(A,B)), holds(P,(B,C)), holds(P,(A,C)).
%% antitransitive_type_check(P):- body_pred(P,_), not type(P,_).
%% antitransitive_type_check(P):- type(P,(TA,TB)), type(P,(TB,TC)), type(P,(TA,TC)).

%% prop(antitriangular,P):- antitriangular_type_check(P), body_pred(P,2), not antitriangular_aux(P).
%% antitriangular_aux(P):- antitriangular_type_check(P), holds(P,(A,B)), holds(P,(B,C)), holds(P,(C,A)).
%% antitriangular_type_check(P):- body_pred(P,_), not type(P,_).
%% antitriangular_type_check(P):- type(P,(TA,TB)), type(P,(TB,TC)), type(P,(TC,TA)).

%% %% pab and qbc implies rac
%% prop(pab_qbc_implies_rac,(P,Q,R)):- holds(P,(A,B)), holds(Q,(B,C)), holds(R,(A,C)), not pab_qbc_implies_rac_aux(P,Q,R).
%% pab_qbc_implies_rac_aux(P,Q,R):- holds(P,(A,B)), holds(Q,(B,C)), holds(R,(_,_)), not holds(R,(A,C)).

%% %% pab and qbc implies rac
%% prop(pab_qac_implies_rbc,(P,Q,R)):- holds(P,(A,B)), holds(Q,(A,C)), holds(R,(B,C)), not pab_qac_implies_rbc_aux(P,Q,R).
%% pab_qac_implies_rbc_aux(P,Q,R):- holds(P,(A,B)), holds(Q,(A,C)), holds(R,(_,_)), not holds(R,(B,C)).

%% p(A) and q(B) implies r(A,B)
%% prop(pa_qb_implies_rab,(P,Q,R)):- holds(P,(A,)), holds(Q,(B,)), holds(R,(A,B)), holds(P,(X,)), holds(Q,(Y,)), #count{X,Y: not holds(R,(X,Y))} == 0.

%% prop(chain,(P,Q)):- type(P,(_,Ta)), type(Q,(Ta,_)), body_pred(P,_), body_pred(Q,_), not chain_sat((P,Q)).
%% chain_sat((P,Q)):- body_pred(P,_), body_pred(Q,_), type(P,(_,Ta)), type(Q,(Ta,_)), holds(P,(_,A)), holds(Q,(A,_)).

%% prop(pre_postcon,(P,Q,R)):- type(P,(Ta,)), type(Q,(Ta,Tb)), type(R,(Tb,)), body_pred(P,_), body_pred(Q,_), body_pred(R,_), not pre_postcon_sat((P,Q,R)).
%% pre_postcon_sat((P,Q,R)):-
%%     type(P,(Ta,)),
%%     type(Q,(Ta,Tb)),
%%     type(R,(Tb,)),
%%     body_pred(P,_),
%%     body_pred(Q,_),
%%     body_pred(R,_),
%%     holds(P,(A,)),
%%     holds(Q,(A,B)),
%%     holds(R,(B,)).