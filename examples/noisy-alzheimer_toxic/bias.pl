%% downloaded from https://www.doc.ic.ac.uk/~shm/Datasets/
%% used in the following paper
%% @article{King:1996:10.1007/BF03037220,
%% author = {King, RD and Srinivasan, A and Sternberg, MJE},
%% doi = {10.1007/BF03037220},
%% journal = {New Generation Computing},
%% title = {Relating chemical activity to structure: An examination of ILP successes},
%% url = {http://dx.doi.org/10.1007/BF03037220},
%% volume = {14},
%% year = {1996}
%% }

head_pred(less_toxic,2).
body_pred(x_subst,3).
body_pred(alk_groups,2).
body_pred(r_subst_1,2).
body_pred(r_subst_2,2).
body_pred(r_subst_3,2).
body_pred(ring_substitutions,2).
%body_pred(ring_subst_1,2).
body_pred(ring_subst_2,2).
body_pred(ring_subst_3,2).
body_pred(ring_subst_4,2).
body_pred(ring_subst_5,2).
body_pred(ring_subst_6,2).
body_pred(polar,2).
body_pred(size,2).
body_pred(flex,2).
body_pred(h_doner,2).
body_pred(h_acceptor,2).
body_pred(pi_doner,2).
body_pred(pi_acceptor,2).
body_pred(polarisable,2).
body_pred(sigma,2).
body_pred(n_val,2).
body_pred(gt,2).
body_pred(great_polar,2).
body_pred(great_size,2).
body_pred(great_flex,2).
body_pred(great_h_don,2).
body_pred(great_h_acc,2).
body_pred(great_pi_don,2).
body_pred(great_pi_acc,2).
body_pred(great_polari,2).
body_pred(great_sigma,2).


type(less_toxic,(a,a)).
type(x_subst,(a,n,b)).
type(alk_groups,(a,n)).
type(r_subst_1,(a,l)).
type(r_subst_2,(a,m)).
type(r_subst_3,(a,n)).
type(ring_substitutions,(a,n)).
%type(ring_subst_1,(a,b)).
type(ring_subst_2,(a,b)).
type(ring_subst_3,(a,b)).
type(ring_subst_4,(a,b)).
type(ring_subst_5,(a,b)).
type(ring_subst_6,(a,b)).
type(polar,(b,c)).
type(size,(b,d)).
type(flex,(b,e)).
type(h_doner,(b,f)).
type(h_acceptor,(b,g)).
type(pi_doner,(b,h)).
type(pi_acceptor,(b,i)).
type(polarisable,(b,j)).
type(sigma,(b,k)).
type(n_val,(a,n)).
type(gt,(n,n)).
type(great_polar,(c,c)).
type(great_size,(d,d)).
type(great_flex,(e,e)).
type(great_h_don,(f,f)).
type(great_h_acc,(g,g)).
type(great_pi_don,(h,h)).
type(great_pi_acc,(i,i)).
type(great_polari,(j,j)).
type(great_sigma,(k,k)).

direction(less_toxic,(in,in)).
direction(x_subst,(in,out,out)).
direction(alk_groups,(in,out)).
direction(r_subst_1,(in,out)).
direction(r_subst_2,(in,out)).
direction(r_subst_3,(in,out)).
direction(ring_substitutions,(in,out)).
%direction(ring_subst_1,(in,out)).
direction(ring_subst_2,(in,out)).
direction(ring_subst_3,(in,out)).
direction(ring_subst_4,(in,out)).
direction(ring_subst_5,(in,out)).
direction(ring_subst_6,(in,out)).
direction(polar,(in,out)).
direction(size,(in,out)).
direction(flex,(in,out)).
direction(h_doner,(in,out)).
direction(h_acceptor,(in,out)).
direction(pi_doner,(in,out)).
direction(pi_acceptor,(in,out)).
direction(polarisable,(in,out)).
direction(sigma,(in,out)).
direction(n_val,(in,out)).
direction(gt,(in,out)).
direction(great_polar,(in,out)).
direction(great_size,(in,out)).
direction(great_flex,(in,out)).
direction(great_h_don,(in,out)).
direction(great_h_acc,(in,out)).
direction(great_pi_don,(in,out)).
direction(great_pi_acc,(in,out)).
direction(great_polari,(in,out)).
direction(great_sigma,(in,out)).
