## Chapter 1. All You Need is λ

* *calculus*
* *lambda calculus*

* *functional programming*: programs are a combination of *expressions*.
* *express* include concrete values, variables, and *functions*.
* *purity* = *referential transparency*: the same function and the same values to evaluate, will always return the same result.

* *lambda terms*
  * expressions
  * variables
  * abstractions
    * *head* `λx`
    * *body* ...expression

* **α-equivalence**
  * `λx.x`
  * `λy.y`
  * `λz.z`
  
* **β-reduction**
  1. `(λx.x) (λy.y) z`
  2. `(λy.y) z`
  2. `z`

* *bound-varialbes* and *free-variables*

* *normal form*

* *combinators*: lambda term with no free variables.

* divergence:
  * the Ω combinator: `(λx.x x) (λx.x x)`
