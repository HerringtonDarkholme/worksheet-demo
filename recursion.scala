// To use recursion, define a trait with a type declaration.
// A type declaration is an abstract type that can have type parameters and bounds.
// Then, define a subtrait that implements the type declaration.
// Recursion is allowed in this implementation, with restrictions that are discussed later.


// abstract types
trait Recurse {
  // Next: next argument passed tto recursive function
  type Next <: Recurse
  // X: Recursive Function
  type X[R <: Recurse] <: Int
}

// Implementation
trait RecurseA extends Recurse {
  type Next = RecurseA
  type X[R <: Recurse] = R#X[R#Next]
}

// These lines won't work
trait R {
  type X[Next <: R] = Next#X[Next]
  // Here Next#X directly refer to R
  // so ti will incur Cyclic Reference Error
}
