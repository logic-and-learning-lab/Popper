#defined does/2.
#defined my_input/1.

c_p(p).
c_q(q).
c_r(r).
c_a(a).
c_b(b).
c_c(c).
my_succ(1,2).
my_succ(2,3).
my_succ(3,4).
my_succ(4,5).
my_succ(5,6).
my_succ(6,7).

const(p).
const(q).
const(r).
const(a).
const(b).
const(c).

not_my_true(A):- const(A), not my_true(A).

role(robot).

my_input(robot,a).
my_input(robot,b).
my_input(robot,c).

%% my_true(6).
%% my_true(p).
%% my_true(r).