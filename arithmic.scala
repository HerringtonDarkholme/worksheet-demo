:load peano.scala


// once Fold is implemented, we can do recursion on Nat
/*
Reference
val list(n: Int) = (n to 1 by -1).toList
def succ(a: Int) = a+1
def add(a: Int, b: Int) =
   ( list(a) :\ b) { (n, acc) => succ(acc) }
def mult(a: Int, b: Int) =
   ( list(a) :\ 0) { (n, acc) => add(acc, b) }
def exp(a: Int, b: Int) =
   (list(b) :\ 1 ) { (n, acc) => mult(acc, a) }
def fact(a: Int) =
   (list(a) :\ 1) { (n, acc) => mult(n, acc) }
def mod(a: Int, b: Int) =
   (list(a) :\ 0 ) { (n, acc) => if(acc+1 == b) 0 else acc+1 }
*/

type Inc = Fold[Nat, Nat] {
  type Apply[N <: Nat, Acc <: Nat] = Succ[Acc]
}
type Add[A <: Nat, B <: Nat] = A#FoldR[B, Nat, Inc]

type Sum[By <: Nat] = Fold[Nat, Nat] {
  type Apply[N <: Nat, Acc <: Nat] = Add[By, Acc]
}
type Mult[A <: Nat, B <: Nat] = A#FoldR[_0, Nat, Sum[B]]

type ProdBy[By <: Nat] = Fold[Nat, Nat] {
  type Apply[N <: Nat, Acc <: Nat] = Mult[By, Acc]
}
type Exp[A <: Nat, B <: Nat] = B#FoldR[_1, Nat, ProdBy[A]]

type Prod = Fold[Nat, Nat] {
  type Apply[N <: Nat, Acc <: Nat] = Mult[N, Acc]
}
type Fact[A <: Nat, B <: Nat] = A#FoldR[_1, Nat, Prod]

type ModFold[By <: Nat] = Fold[Nat, Nat] {
  type Wrap[Acc <: Nat] = By#Compare[Acc]#eq
  type Apply[N <: Nat, Acc <: Nat] = Wrap[Succ[Acc]]#If[_0, Succ[Acc], Nat]
}
type Mod[A <: Nat, B <: Nat] = A#FoldR[_0, Nat, ModFold[B]]


type Sq[N <: Nat] = Exp[N, _2]
type Eq[A <: Nat, B <: Nat] = A#Compare[B]#eq

toInt[_4]
toInt[Add[_1, _2]]
toBoolean[Eq[Sq[_9], Add[_1, Mult[_8, _10]]]]
