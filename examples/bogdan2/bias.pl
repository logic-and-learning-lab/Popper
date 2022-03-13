max_clauses(3).
max_vars(3).
max_body(2).
enable_recursion.
enable_pi.

head_pred(f3,2).
body_pred(is_empty,1).
body_pred(not_empty,1).
body_pred(is_space,1).
body_pred(not_space,1).
body_pred(is_uppercase,1).
body_pred(not_uppercase,1).
body_pred(is_lowercase,1).
body_pred(not_lowercase,1).
body_pred(is_letter,1).
body_pred(not_letter,1).
body_pred(is_number,1).
body_pred(not_number,1).
body_pred(skip1,2).
body_pred(copy1,2).
body_pred(copyskip1,2).
body_pred(mk_uppercase,2).
body_pred(mk_lowercase,2).
body_pred(copyskiprest,2).

direction(copyskiprest,(in,out)).
direction(f0,(in,out)).
direction(f1,(in,out)).
direction(f2,(in,out)).
direction(f3,(in,out)).
direction(f4,(in,out)).
direction(f5,(in,out)).
direction(f6,(in,out)).
direction(is_empty,(in,)).
direction(not_empty,(in,)).
direction(is_space,(in,)).
direction(not_space,(in,)).
direction(is_uppercase,(in,)).
direction(not_uppercase,(in,)).
direction(is_lowercase,(in,)).
direction(not_lowercase,(in,)).
direction(is_letter,(in,)).
direction(not_letter,(in,)).
direction(is_number,(in,)).
direction(not_number,(in,)).
direction(skip1,(in,out)).
direction(copy1,(in,out)).
direction(copyskip1,(in,out)).
direction(mk_uppercase,(in,out)).
direction(mk_lowercase,(in,out)).



direction(inv1,(in,out)).