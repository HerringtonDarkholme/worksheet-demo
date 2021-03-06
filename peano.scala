:load comparison.scala


import scala.language.higherKinds

/*
 we can do recursion at the type-level in Scala.
 The first application for this will be representing numbers
 in the type system (Peano numbers).
 One use of these is type-safe indexing into HLists.
 */

/*
compare to value level
trait Fold[-Elem, Value] {
  def apply(e: Elem, v: Value): Value
}
*/

trait Fold[-Elem, Value] {
  type Apply[E <: Elem, V <: Value] <: Value
}

// the basic idea here is straight
// first define a basic abstract class
// then implement number theory by extending

// To support numeric operation on Nat
// Nat needs a Match type as Bool has a If
// Match has two choices to return
// Match will distinguish the number itself
// if number is 0, then return the Zero choice
// if number is not 0, then return the nonZero choice

// The tricky part is the presentation of nonZero choice
// nonZero should be a type constructor that takes Nat as arg
// so Match can get the N in Succ[N], which enables recursion
// that encodes addition, substraction or other.
// also type constructor give the map power to Nat
// _2 => Int :: Int :: HNil, mapping Nat to HList

sealed trait Nat {

  type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] <: Up

  type Compare[N <: Nat] <: Comparison

  type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] <: Type
}

sealed trait _0 extends Nat {

  type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] = IfZero

  // 0 compare other number, if N > 0, return ConstLT[N]
  // otherwise EQ. Notice ConstLT should be a type-constructor
  type Compare[N <: Nat] = N#Match[ConstLT, EQ, Comparison]

  type ConstLT[A] = LT

  type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] = Init
}

sealed trait Succ[N <: Nat] extends Nat {
  type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] = NonZero[N]

  // if other is Zero, return GT
  // otherwise recurse, compare O-1 and N
  type Compare[O <: Nat] = O#Match[N#Compare, GT, Comparison]
  // that is Compare always return Comparison
  // and Compare is not a cyclic reference

  type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] =
    F#Apply[Succ[N], N#FoldR[Init, Type, F]]
}

// forget about macro, it's too heavy to implement for this
type _1 = Succ[_0]
type _2 = Succ[_1]
type _3 = Succ[_2]
type _4 = Succ[_3]
type _5 = Succ[_4]
type _6 = Succ[_5]
type _7 = Succ[_6]
type _8 = Succ[_7]
type _9 = Succ[_8]
type _10 = Succ[_9]

toBoolean[_0#Compare[_0]#eq]
toBoolean[_0#Compare[_0]#lt]
toBoolean[_3#Compare[_4]#le]

type C = _0#FoldR[Int, AnyVal, Fold[Nat, AnyVal]]
implicitly[C =:= Int]

case class NatRep[N <: Nat](value: Int)

def toInt[N <: Nat : NatRep]: Int = implicitly[NatRep[N]].value
implicit val zeroRep = NatRep[_0](0)
implicit def posRep[N <: Nat: NatRep]: NatRep[Succ[N]] =
  NatRep(toInt[N] + 1)

