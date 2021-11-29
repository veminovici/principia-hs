# 6. Nameless Representation of Terms

Represent variables **simbolically** = replace the names of variables with fresh names.

**Barendregt convention** ([link](https://en.wikipedia.org/wiki/De_Bruijn_index#Barendregt_convention))= represent variables symbolically but introduce a general condition that the names of all bound variables must all be different from each other and from any free variable we may use.

**Canonical representation** that does not need renaming

**Avoid** substitution all together by using mechanisms such *explicit substitution* ([link](https://en.wikipedia.org/wiki/Explicit_substitution)).

**Avoid variables** altogether by working in a language directly on combinators.

We will go with **canonical**.

Idea is to make variable occurences **point directly** to their binders rather than reffering them by name. Replace the variable names by natural numbers.

The **number k** stands for *“the variable bound by the k’th enclosing λ.”* 

For example, the ordinary term *λx.x* corresponds to the nameless term *λ.0*, while *λx.λy. x (y x)* corresponds to *λ.λ. 1 (0 1)*. Nameless terms are also sometimes called **de Bruijn terms**, and the numeric variables in them are called **de Bruijn indices**. ([link](https://en.wikipedia.org/wiki/De_Bruijn_notation))

Formally, we define the *syntax of nameless terms* almost exactly like the syntax of ordinary terms. The only difference is that we need to *keep careful track of how many free variables each term may contain*.  We distinguish the sets of terms with no free variables (called the *0-terms*), terms with at most one free variable (*1-terms*), and so on.

**Terms** is the smallest *family of sets* {T0, T1, T2,...} such that
- k in Tn 0 <= k < n
- if t1 in Tn and n > 0 => λx.t1 in Tn-1
- if t1 and t2 in Tn => (t1 t2) in Tn

The elements of Tn are terms with *at most n* free variables, numbered between 0 and n-1.

Each ordinary (closed) term has just one de-Brujin representation and that two ordinary terms are equivalent modulo renaming of bound variables iff they have the same de-Brujin representation.

To deal with terms containing free variables, we need a *naming context*. For example for *λx. y x* we know what to do with x but we cannot see the binder for *y* so it is not clear how *far away* it might be and we dont know what number to assign. The solution is to choose once and for all an assignment (called a naming context) of de-Brujin indeces for free variables and use this assignment consistently when we need to choose numbers for free variables.
