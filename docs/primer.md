# Popper Primer

This is an informal description of how to use Popper, written as I learn how to use it.  It's my hope that it will be generally useful and that it will gel into a coherent form as my experience grows.

I will begin by discussing aspects of Popper use that are harder to extract from the examples and the [Popper paper](http://arxiv.org/abs/2005.02259).

## What goes in a `bias.pl` file?

### Type declarations for predicates

**Important note:** Types are all-or-nothing.  If you supply type information for some predicate, you must supply type information for all, or you will encounter a runtime error in Popper.

### Constant declarations

Constant declarations enable type-checking and, if they are applied exhaustively to types with finite domains, they can significantly improve Popper's efficiency.  There are 2 forms of constant declarations:

    constant(<constant term>, <type name>)
    
for example:
  
    constant(g5, coordinate).

Note that this first form is only usable for *atomic* constant terms, not structured terms (functional expressions).  For the latter, one must use a second form:

    constant(<predicate>, <type name>)
    
for example:

    constant(move_term, act).
    
complemented by the following term in the background knowledge (`bk.pl`):

    move_term(move(B, E)) :- coordinate(B), coordinate(E).
    

