package exercises.week03

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  override def contains(x: Int): Boolean = false

  override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

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

  override def toString: String = "{" + left + elem + right + "}"
}

object Main {
  def main(args: Array[String]): Unit = {
    val t1 = new NonEmpty(3, Empty, Empty)
    val t2 = t1.incl(4)
    val t3 = new NonEmpty(6, Empty, Empty)
    val t4 = t3.incl(8)
    println(t1)
    println(t2)
    println(t2.union(t4))
  }
}
