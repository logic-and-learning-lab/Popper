%% taken from the paper:
%% Andrew Cropper, Richard Evans, Mark Law: Inductive general game playing. Mach. Learn. 109(7): 1393-1434 (2020)
%% https://arxiv.org/pdf/1906.09627.pdf


head_pred(next_value,2).
body_pred(does,3).
body_pred(true_value,2).
body_pred(my_succ,2).
body_pred(press_button,1).
body_pred(noop,1).
body_pred(player,1).
body_pred(c1,1).
body_pred(c2,1).
body_pred(c3,1).
body_pred(c4,1).
body_pred(c5,1).

type(next_value,(ex,int)).
type(does,(ex,agent,action)).
type(true_value,(ex,int)).
type(my_succ,(int,int)).
type(press_button,(action,)).
type(noop,(action,)).
type(player,(agent,)).
type(c1,(int,)).
type(c2,(int,)).
type(c3,(int,)).
type(c4,(int,)).
type(c5,(int,)).

%% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:- clause(C), #count{V : var_type(C,V,ex)} != 1.
