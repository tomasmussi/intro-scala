package assignments.week02

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   * Aca se esta definiendo que FunSet es un tipo funcion que toma un Int y devuelve un Boolean!
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): FunSet = (x => x == elem)


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet = (x => s(x) || t(x))

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = (x => s(x) && t(x))

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet = (x => s(x) && !t(x))

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = (x => s(x) && p(x))


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  def existsEnlarged(s: FunSet, p: Int => Boolean): Boolean = {
    // Need to return a Boolean, as forall
    def predicate(x: Int): Boolean = {
      // We have to negate the predicate
      !p(x)
    }
    !forall(s, predicate)
  }

  /**
   * This is how it should look like iterating... but part of the exercise is to use forall,
   * given that it only exchanges the conditions and eliminates the negation in the predicate
   * */
  def iterexists(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) false
      else if (s(a) && p(a)) true
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet = (x => exists(s, y => f(y) == x))
  //def map(s: FunSet, f: Int => Int): FunSet = (x => !forall(s, y => f(y) != x))

  // Map can be implemented with forall or with exists, so there are two ways.
  /**
   * Map with exists:
   * */
  def mapExists(s: FunSet, f: Int => Int): FunSet = {
    // Need to return a FunSet:
    def newFunSet(x: Int): Boolean = {
      // There has to be a condition to meet
      def predicate(y: Int): Boolean = {
        // There should exist a number, that applying function f, meets the condition necessary in set s,
        // that is, f(y) is equal to some x in set S
        f(y) == x
      }
      exists(s, predicate)
    }
    newFunSet
  }

  /**
   * Map with forall.
   * There is an equivalence between exists and forall, negating a few things we should be able to
   * implement map with forall.
   * */
  def mapForall(s: FunSet, f: Int => Int): FunSet = {
    // Need to return a FunSet:
    def newFunSet(x: Int): Boolean = {
      // There has to be a condition to meet
      def predicate(y: Int): Boolean = {
        // The condition is that for all the elements, none of them meets that f(y) is equal to a member in set
        f(y) != x
      }
      // None of the elements can be in new set
      !forall(s, predicate)
    }
    newFunSet
  }


  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit = {
    println(toString(s))
  }
}

object FunSets extends FunSets
