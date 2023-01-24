head_pred(crashes, 1).
body_pred(greensignal, 1).
body_pred(hastraffic, 1).
body_pred(intersect, 2).

enable_pi.

% solution
% crashable(X, Y) :- intersect(X, Y), greenSignal(X), greenSignal(Y).
% crashes(X) :- hasTraffic(X), crashable(X, Y), hasTraffic(Y).
% crashes(X) :- hasTraffic(X), crashable(Y, X), hasTraffic(Y).