sealed trait Indexed {
  type Before <: HList
  type After <: HList
  type At

  def withIndex[R](F: (Before, At, After) => R): R

  def at =
    withIndex((_, a, _) => a)
  def drop =
    withIndex((_, a, t) => a :: t)
  def take =
    withIndex((b, _, _) => b)
  def replace[A](a: A) =
    withIndex((b, _, t) => b ::: a :: t)
  def remove =
    withIndex((b, _, t) => b ::: t)
  def map[B](f: At => B) =
    withIndex((b, a, t) => b ::: f(a) :: t)
  def flatMap[B <: HList](f: At => B) =
    withIndex((b, a, t) => b ::: f(a) ::: t)
  def insert[C](c: C) =
    withIndex((b, a, t) => b ::: c :: a :: t)
  def insertH[C <: HList](c: C) =
    withIndex((b, a, t) => b ::: c ::: a :: t)
  def splitAt =
    withIndex((b, a, t) => (b, a :: t))
}

final class Indexed0[H, T <: HList](val hc: H :: T) extends Indexed {
  type Before = HNil
  type After = T
  type At = H
  def withIndex[R](f: (Before, At, After) => R): R =
    f(HNil, hc.head, hc.tail)
}

final class IndexedN[H, I <: Indexed](h: H, iTail: I) extends Indexed {
  type Before = H :: I#Before
  type At = I#At
  type After = I#After
  def withIndex[R](f: (Before, At, After) => R): R =
    iTail.withIndex((before, at, after) => f(HCons(h, before), at, after))
}
