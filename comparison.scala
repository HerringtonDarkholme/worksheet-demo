// attention: this is a REPL session
:load bool.scala

// The basic idea is the same
// encoding comparison in three element
// gt, lt, eq, according to the comparison result
// return differenrt value
sealed trait Comparison {
  type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] <: Up

  // quick alias for Bool value
  type gt = Match[False, False, True, Bool]
  type ge = Match[False, True, True, Bool]
  type eq = Match[False, True, False, Bool]
  type le = Match[True, True, False, Bool]
  type lt = Match[True, False, False, Bool]
}

sealed trait GT extends Comparison {
  type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfGT
}

sealed trait LT extends Comparison {
  type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfLT
}

sealed trait EQ extends Comparison {
  type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfEQ
}
