head_pred(shipto, 2).
body_pred(customercity, 2).
body_pred(hasordered, 2).
body_pred(productname, 2).

% solution
% ship_to(ProdName, City) :- has_ordered(CustNo, ProdNo), customer_city(CustNo, City), product_name(ProdNo, ProdName).
