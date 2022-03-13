%% (<= (next (color ?r ?c))
%%       (does ?p (mark ?r ?c)))

%%   (<= (next (color ?r ?c))
%%       (true (color ?r ?c)))

%% pos(next_color(100,r5,blue)).

max_clauses(1).
max_vars(5).
max_body(5).

head_pred(next_color,3).
body_pred(does_mark,4).
body_pred(my_input_mark,4).
body_pred(my_true_color,3).

type(next_color,(ex,pos,hue)).
type(does_mark,(ex,agent,pos,hue)).
type(my_input_mark,(ex,agent,pos,hue)).
type(my_true_color,(ex,pos,hue)).


%% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.