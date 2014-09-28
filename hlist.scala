:load peano.scala

trait Fold[-Elem, Value] {
  type Apply[N <: Elem, Acc <: Value] <: Value
  def apply[N <: Elem, Acc <: Value](n: N, acc: Acc): Apply[N, Acc]
}

type Inc = Fold[Any, Nat] {
  type Apply[N <: Any, Acc <: Nat] = Succ[Acc]
}


sealed trait HList {
  type Head
  type Tail <: HList
  type Length = Foldr[Nat, Inc, _0]
  type Wrap[M[_]] <: HList

  type Foldr[Value, F <: Fold[Any, Value], I <: Value] <: Value
  def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldr[Value, F, I]
  type Foldl[Value, F <: Fold[Any, Value], I <: Value] <: Value
  def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldl[Value, F, I]

  type toI[N <: Nat] <: Indexed
}

:load indexed.scala

final case class HCons[H, T <: HList](head: H, tail: T) extends HList {

  type Wrap[M[_]] = HCons[M[H], T#Wrap[M]]
  type Head = H
  type Tail = T
  def ::[T](v: T) = HCons(v, this)

  type Foldr[Value, F <: Fold[Any, Value], I <: Value] =
    F#Apply[H, tail.Foldr[Value, F, I]]
  def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldr[Value, F, I] =
    f(head, tail.foldr[Value, F, I](f, i))

  type Foldl[Value, F <: Fold[Any, Value], I <: Value] =
    tail.Foldl[Value, F, F#Apply[H, I]]
  def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldl[Value, F, I] =
    tail.foldl[Value, F, F#Apply[H, I]](f, f(head, i))

  // match on N
  // if it is _0 then point to this cell
  // otherwise index to the right
  type toI[N <: Nat] = N#Match[IN, Indexed0[H, T], Indexed]
  type IN[M <: Nat] = IndexedN[H, tail.toI[M]]

  override def toString = s"$head :: $tail"
}

sealed class HNil extends HList {

  type Head = Nothing
  type Tail = HNil
  type Wrap[M[_]] = HNil

  def ::[T](v: T) = HCons(v, this)

  type Foldr[Value, F <: Fold[Any, Value], I <: Value] = I
  def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldr[Value, F, I] = i
  type Foldl[Value, F <: Fold[Any, Value], I <: Value] = I
  def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldl[Value, F, I] = i

  type toI[N <: Nat] = Nothing

  override def toString = "HNil"
}

case object HNil extends HNil

type ::[H, T <: HList] = HCons[H, T]
object :: {
  def unapply[H, T <: HList](list: HCons[H, T]) = Some((list.head, list.tail))
}

type Length[H <: HList] =
  H#Foldr[Nat, Inc, _0]

// use object to include both type level and value level
object AppHCons extends Fold[Any, HList] {
  type Apply[N <: Any, H <: HList] = N :: H
  def apply[N, H <: HList](n: N, h: H) = HCons(n, h)
}
type :::[A <: HList, B <: HList] =
  A#Foldr[HList, AppHCons.type, B]

type Reverse[A <: HList] =
  A#Foldl[HList, AppHCons.type, HNil]

object Length extends  Fold[Any, Int] {
  type Apply[N <: Any, Acc <: Int] = Int
  def apply[N, Acc <: Int](n: N, a: Acc) = a + 1
}

sealed trait HListOps[B <: HList] {
  def length: Int
  def :::[A <: HList](a: A): A ::: B
  def reverse: Reverse[B]
  def i[N <: Nat](implicit in: HList => toI[N]) = in(this)
}

implicit def hlistOps[B <: HList](b: B): HListOps[B] = new HListOps[B] {
  def length = b.foldr(Length, 0)
  def reverse = b.foldl[HList, AppHCons.type, HNil](AppHCons, HNil)
  def :::[A <: HList](a: A): A#Foldr[HList, AppHCons.type, B] =
    a.foldr[HList, AppHCons.type, B](AppHCons, b)

  implicit def indexed0[H, T <: HList](hc: H :: T): Indexed0[H, T] =
    new Indexed0[H, T](hc)
  implicit def indexedN[H, T <: HList](hc: H :: T)(implicit iTail: T => I): IndexedN[H, I] =
    new IndexedN[H, I](hc.head, iTail(hc.tail))
}
