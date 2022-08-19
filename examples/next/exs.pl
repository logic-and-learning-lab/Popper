pos(next_list([i, f, x, c, d, c],c)).
pos(next_list([g, b, c, d, x, b, i, j],b)).
pos(next_list([b,d,h,j, x, b],b)).
pos(next_list([h,e,g,h,j,c,x,g],g)).
pos(next_list([a,f,k,d,e,x,d,a,b,c],d)).
pos(next_list([a,e,d,x,f,a],f)).
pos(next_list([d,e,f,x,h,c,a],h)).

neg(next_list([c, d, i,j],d)).
neg(next_list([a, j, c,e, b,c,d],b)).
neg(next_list([c,e, i, a, b, b, a],e)).
neg(next_list([c,b,c,d,c,d,c,f],d)).

