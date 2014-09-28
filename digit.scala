:load comparison.scala

/*
An alternative is to use a binary representation of numbers,
called Dense here after the implementation in Okasaki's Purely Functional Data Structures
*/
sealed trait Digit {
  type Match[IfOne <: Up, IfZero <: Up, Up] <: Up
  type Compare[D <: Digit] <: Comparison
}


// First, we need a Digit type to represent a bit.
// It has two subtypes, One and Zero.
sealed trait Zero extends Digit {
  type Match[IfOne <: Up, IfZero <: Up, Up] = IfZero
  type Compare[D <: Digit] = D#Match[LT, EQ, Comparison]
}

sealed trait One extends Digit {
  type Match[IfOne <: Up, IfZero <: Up, Up] = IfOne
  type Compare[D <: Digit] = D#Match[EQ, GT, Comparison]
}


// Next, we create the Dense type.
// It is a type level heterogeneous list with element types constrained to be Digits.
// The head of the list is the least significant bit.
// The last element of a non-empty list is always One and is the most significant bit.
// An empty list represents zero.

sealed trait Dense {
  type digit <: Digit
  type tail <: Dense
  type Inc <: Dense
  type ShiftR <: Dense
  type ShiftL <: Dense
}

sealed trait DCons[d <: Digit, T <: Dense] extends Dense {
  type digit = d
  type tail = T
  type Inc = d#Match[DCons[Zero, T#Inc], DCons[One, T], Dense]
  type ShiftR = T
  type ShiftL = DCons[Zero, DCons[d, T]]
}

sealed trait DNil extends Dense {
  type digit = Nothing
  type tail = Nothing
  type Inc = DCons[One, DNil]
  type ShiftR = DNil
  type ShiftL = DNil
}

type ::[H <: Digit, T <: Dense] = DCons[H, T]
type _0 = DNil
type _1  = One :: DNil
type _2 = Zero :: One :: DNil
type _3 = One :: One :: DNil
type _4 = Zero :: Zero :: One :: DNil
type _5 = One :: Zero :: One :: DNil
type _6 = _5#Inc


case class DRep[D <: Dense](value: Int)

def toInt[D <: Dense: DRep]: Int = implicitly[DRep[D]].value

implicit def dnilToRep = DRep[DNil](0)
implicit def dcons0ToRep[D <: Dense: DRep]: DRep[DCons[Zero, D]] =
  DRep(implicitly[DRep[D]].value * 2)
implicit def dcons1ToRep[D <: Dense: DRep]: DRep[DCons[One, D]] =
  DRep(implicitly[DRep[D]].value * 2 + 1)

toInt[_5]
