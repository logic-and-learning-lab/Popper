max_vars(6).
max_body(7).
enable_recursion.

head_pred(next_list,2).
body_pred(head,2).
body_pred(tail,2).
body_pred(x,1).
body_pred(a,1).
body_pred(b,1).
body_pred(c,1).
body_pred(d,1).
body_pred(e,1).
body_pred(f,1).
body_pred(g,1).
body_pred(h,1).
body_pred(i,1).
body_pred(j,1).

type(next_list,(list,element)).
type(head,(list,element)).
type(tail,(list,list)).
type(x,(element,)).
type(a,(element,)).
type(b,(element,)).
type(c,(element,)).
type(d,(element,)).
type(e,(element,)).
type(f,(element,)).
type(g,(element,)).
type(h,(element,)).
type(i,(element,)).
type(j,(element,)).

direction(next_list,(in,out)).
direction(head,(in, out)).
direction(tail,(in, out)).
direction(x,(in,)).
direction(a,(in,)).
direction(b,(in,)).
direction(c,(in,)).
direction(d,(in,)).
direction(e,(in,)).
direction(f,(in,)).
direction(g,(in,)).
direction(h,(in,)).
direction(i,(in,)).
direction(j,(in,)).
