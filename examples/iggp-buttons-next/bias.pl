%% taken from the paper:
%% Andrew Cropper, Richard Evans, Mark Law: Inductive general game playing. Mach. Learn. 109(7): 1393-1434 (2020)
%% https://arxiv.org/pdf/1906.09627.pdf
%% Three buttons and three lights. The first button toggles the first light. The second button interchanges first and second lights. The third button interchanges the second and third lights. All lights initially off. The goal is to get them all on. The game terminates when all three lights are on or after 6 steps, whichever comes first.

head_pred(next,2).
body_pred(does,3).
body_pred(my_input,2).
body_pred(my_true,2).
body_pred(my_succ,2).
body_pred(role,1).
body_pred(c_p,1).
body_pred(c_q,1).
body_pred(c_r,1).
body_pred(c_a,1).
body_pred(c_b,1).
body_pred(c_c,1).
body_pred(not_my_true,2).

type(next,(ex,prop)).
type(does,(ex,agent,action)).
type(my_input,(agent,action)).
type(my_true,(ex,prop)).
type(my_succ,(prop,prop)).
type(role,(agent,)).
type(c_p,(prop,)).
type(c_q,(prop,)).
type(c_r,(prop,)).
type(c_a,(action,)).
type(c_b,(action,)).
type(c_c,(action,)).
type(not_my_true,(ex,prop)).

%% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : var_type(C,V,ex)} != 1.