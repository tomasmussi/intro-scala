package assignments.week02

object Main extends App {
  import FunSets._

  println("Singleton 1 contains 1 " + contains(singletonSet(1), 1))
  println("Singleton 1 contains 2 " + contains(singletonSet(1), 2))

  def f(x: Int): Boolean = x % 2 == 0
  def notf(x: Int): Boolean = !f(x)

  def evenSet(x: Int): Boolean = x % 2 == 0
  def oddSet(x: Int): Boolean = !evenSet(x)
  def positiveSet(x: Int): Boolean = x > 0
  def failingSet(x: Int): Boolean = (x == 1 || x == 3 || x == 4 || x == 5 || x == 7 || x == 1000)

  println("Is 4 even? " + f(4))
  println("Is 3 even? " + f(3))

  val all = union(f, notf)
  println("Is 4 in set of all odd and even? " + all(4))
  println("Is 3 in set of all odd and even? " + all(3))

  println(exists(evenSet, (x => x == 1)))

  val odd = map(evenSet, (x => x + 1))
  println(contains(odd, 1))

  printSet(failingSet)
  val fa = map(failingSet, (x => x - 1))
  printSet(fa)
}
