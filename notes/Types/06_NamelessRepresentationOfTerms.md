# 6. Nameless Representation of Terms

Represent variables **simbolically** = replace the names of variables with fresh names.

**Barendregt convention** ([link](https://en.wikipedia.org/wiki/De_Bruijn_index#Barendregt_convention))= represent variables symbolically but introduce a general condition that the names of all bound variables must all be different from each other and from any free variable we may use.

**Canonical representation** that does not need renaming

**Avoid** substitution all together by using mechanisms such *explicit substitution* ([link](https://en.wikipedia.org/wiki/Explicit_substitution)).

**Avoid variables** altogether by working in a language directly on combinators.

We will go with **canonical**.

Idea is to make variable occurences **point directly** to their binders rather than reffering them by name. Replace the variable names by natural numbers.

The number k stands for *“the variable bound by the k’th enclosing λ.”* For example, the ordinary term *λx.x* corresponds to the nameless term *λ.0*, while *λx.λy. x (y x)* corresponds to *λ.λ. 1 (0 1)*. Nameless terms are also sometimes called **de Bruijn terms**, and the numeric variables in them are called **de Bruijn indices**. ([link](https://en.wikipedia.org/wiki/De_Bruijn_notation))

Formally, we define the *syntax of nameless terms* almost exactly like the syntax of ordinary terms. The only difference is that we need to *keep careful track of how many free variables each term may contain*.  We distinguish the sets of terms with no free variables (called the *0-terms*), terms with at most one free variable (*1-terms*), and so on. 
