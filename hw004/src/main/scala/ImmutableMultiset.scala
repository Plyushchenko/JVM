import scala.collection.{AbstractIterable, GenTraversableOnce}

class ImmutableMultiset[T](elements: Map[T, Int] = Map.empty[T, Int]) extends AbstractIterable[T] {
  private val numberOfElements: Int = elements.values.sum

  override def size: Int = numberOfElements

  override def isEmpty: Boolean = size == 0

  override def nonEmpty: Boolean = !isEmpty

  override def toSeq: Seq[T] = elements.flatMap(x => Seq.fill(x._2)(x._1)).toSeq

  def count(value: Any): Int = {
    value match {
      case t: T => elements.getOrElse(t, 0)
      case _ => 0
    }
  }

  def apply(value: Any): Int = count(value)

  def contains(value: Any): Boolean = count(value) != 0

  def +(value: T): ImmutableMultiset[T] =
      ImmutableMultiset(elements + (value -> (count(value) + 1)))

  def -(value: T): ImmutableMultiset[T] = {
    if (count(value) > 1)
      ImmutableMultiset(elements + (value -> (count(value) - 1)))
    else
      ImmutableMultiset(elements - value)
  }

  def &(other: ImmutableMultiset[T]): ImmutableMultiset[T] = ImmutableMultiset(other.toSeq
    .intersect(toSeq):_*)

  def |(other: ImmutableMultiset[T]): ImmutableMultiset[T] = {
    val x = other.toSeq.intersect(toSeq)
    val y = other.toSeq.union(toSeq)
    ImmutableMultiset(y.diff(x):_*)
  }

  override def find(p: T => Boolean): Option[T] = elements.keys.find(p)

  override def filter(p: T => Boolean):ImmutableMultiset[T] = ImmutableMultiset(elements
      .filterKeys(p))

  def map[U >: T, R](f: U => R): ImmutableMultiset[R] = ImmutableMultiset(elements
      .map(x => (f(x._1), x._2)))

  def flatMap[U >: T, R](f: U => GenTraversableOnce[R]) = ImmutableMultiset(toSeq.flatMap(f))

  override def iterator: Iterator[T] = toSeq.iterator

  override def equals(other: Any): Boolean = {
    other match {
      case otherMultiset: ImmutableMultiset[T] =>
        otherMultiset.size == size &&
          elements.keys.forall(key => count(key) == otherMultiset.count(key))
      case _ => false
    }
  }
}

object ImmutableMultiset {
  def apply[T](): ImmutableMultiset[T] = new ImmutableMultiset[T]()

  def apply[T](elements: Map[T, Int]): ImmutableMultiset[T] = new ImmutableMultiset[T](elements)

  def apply[T](values: T*): ImmutableMultiset[T] = ImmutableMultiset(values.groupBy(identity)
    .mapValues(_.size))

  def unapplySeq[T](immutableMultiset: ImmutableMultiset[T]): Option[Seq[T]] = Some(
    immutableMultiset.toSeq)

  def empty[T]: ImmutableMultiset[T] = ImmutableMultiset()
}
