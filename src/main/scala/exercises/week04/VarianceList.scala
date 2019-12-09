package exercises.week04

/**
 * Variance in Scala is the way that you can define generic collections and make that a
 * subtype of a collection inherits a supertype of the collection, given the class hierarchy
 * of the objects you are using inside the collection
 *
 * VarianceList[+T]: this class is covariant
 * VarianceList[-T]: this class is contravariant (stores superclasses of T, not subclasses)
 * VarianceList[T]: this class is non covariant, no hierarchy in elements that are stored
 * */
trait VarianceList[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: VarianceList[T]

  /**
   * U is a supertype of T. It is mandatory that in a covariant class, all function parameters
   * are supertypes of the class type.
   * */
  def prepend[U >: T](elem: U): VarianceList[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: VarianceList[T]) extends VarianceList[T] {
  override def isEmpty: Boolean = false
}

object Nil extends VarianceList[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class Empty extends IntSet {
  override def contains(x: Int): Boolean = false
  override def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  override def union(other: IntSet): IntSet = other
  override def toString: String = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  override def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def union(other: IntSet): IntSet = {
    ((left union right) union other) incl elem
  }

}

object test {
  def main(args: Array[String]): Unit = {
    val x: VarianceList[String] = Nil
    def f(xs: VarianceList[NonEmpty], x: Empty): VarianceList[IntSet] = xs prepend x

  }

}
