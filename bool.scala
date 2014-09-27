import scala.language.higherKinds
// introductory example to type level
// church encoding of booleans

// To express Boolean, we can construct a two choices
// if the boolean is true, return the first choice
// otherwise, return the second choice
// In scala a third type is needed
// to enable compiler to infer returning type

sealed trait Bool {
  type If[T <: Up, F <: Up, Up] <: Up
}

sealed trait True extends Bool {
  type If[T <: Up, F <: Up, Up] = T
}

sealed trait False extends Bool {
  type If[T <: Up, F <: Up, Up] = F
}

type Rep[A <: Bool] = A#If[Int, Long, AnyVal]

implicitly[Rep[True] =:= Int]
implicitly[Rep[False] =:= Long]

// extra higherkinds for boolean computation
// &&: if A is True, then return B
//     or return False
// ||: if A is True, then return True
//     or return B
// Not: invert case

type &&[A <: Bool, B <: Bool] = A#If[B, False, Bool]
type ||[A <: Bool, B <: Bool] = A#If[True, B, Bool]
type Not[A <: Bool] = A#If[False, True, Bool]

implicitly[True && False || Not[False] =:= True]

// convert class to boolean
// it can be done via type casting or pattern matching
// but a more 'compile time' solution is implicit
// construct a case class whose value is target
// and take a type as type-constructor's argument

case class BoolRep[B <: Bool](value: Boolean)
implicit val TrueRep = BoolRep[True](true)
implicit val FalseRep = BoolRep[False](false)

def toBoolean[B <: Bool : BoolRep]: Boolean = implicitly[BoolRep[B]].value

toBoolean[True]
toBoolean[False]
toBoolean[True && False || Not[False]]
