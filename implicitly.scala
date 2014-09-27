// The implicit that we want in this case is A =:= B for some types A and B.
// A =:= B will only be found when A is the same type as B.

// implicitly itself is implemented like this
// def implicitly[T](implicit e: T): T = e
// This line tricks compiler to find implicit value for us

// compiles!
implicitly[Int =:= Int]

// but not this
// implictly[Int =:= String]

/*
Let's go astray to a shadowed trod
some implementation details:
Actually =:= is just a Higher-kinded class in scala.
It is somewhat like Map[A, B], that is,
=:= is defined like
  class =:=[A, B]

so in the implictly's bracket, `Int =:= Int` is just a type
`A =:= B` is the infix form of type parameterization for
non-alphanumeric identifier. It is equivalent to =:=[A, B]

so one can define implicts for =:=, so that compiler can find

implicit def EqualTypeEvidence[A]: =:=[A, A] = new =:=[A, A]

So, when `implictly[A =:= B]` is compiled,
compiler tries to find the correct implicit evidence.

If and only If A and B are the same, say Int, the compiler can find
=:=[Int, Int], by the result of implicit function EqualTypeEvidence[Int]

More compelling is <:<, the conformance evidence,
it leverages variance annotation in scala

class <:<[-A, +B]
implict def Conformance[A]: <:<[A, A] = new <:<[A, A]

Consider, when `String <:< java.io.Serializable` is needed,
compiler tries to find an instance of <:<[String, j.i.Serializable]
It can only find instance of the type <:<[String, String]
(or another alternative <:<[Serializable, Serializable])
But given the variance annotation of <:<,
since String is the very type String
and String is a subtype of Serializable and B is in a covariant position
, or, in another direction
snice Serializable is a supertype of String and A is in a contravariant position
and Serializable is the very type Serializable


<:<[String, String] is a subtype of <:<[String, Serializable]
So compiler finds the correct implicit instance as the evidence that
String is a subtype of Serializable. By the principle of subtype subsititution.
(Liskov)

Similarly we can define
Conversion evidence

class <%<[A <% B, B]
implicit def Conversion[A, B] = new <%<[A, B]

Contra-conformance
class >:>[+A, -B]
implicit def Contra[A] = new >:>[A, A]
*/


class <%<[A <% B, B]
implicit def Conversion[A <% B, B] = new <%<[A, B]
implicitly[Int <%< Long]

class >:>[+A, -B]
implicit def Contra[A] = new >:>[A, A]
implicitly[AnyVal >:> Int]
