package assignments.week02

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    def evenSet(x: Int): Boolean = x % 2 == 0
    def oddSet(x: Int): Boolean = !evenSet(x)
    def positiveSet(x: Int): Boolean = x > 0
    def greaterThan50Set(x: Int): Boolean = x > 50
    def boundedSet(x: Int): Boolean = (x >= -1000 && x <= 1000)

    def failingSet(x: Int): Boolean = (x == 1 || x == 3 || x == 4 || x == 5 || x == 7 || x == 1000)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  /**
   * Tests basic contains  with function Sets
   * */
  @Test def `Basic test of new function and trait`: Unit = {
    new TestSets {
      assert(contains(evenSet, 4))
      assert(!contains(evenSet, 3))
      assert(contains(positiveSet, 10))
      assert(!contains(greaterThan50Set, 10))
      assert(!contains(oddSet, 10))
    }
  }

  @Test def `Union`: Unit = {
    new TestSets {
      val evenWith1 = union(evenSet, s1)
      val all = union(evenSet, oddSet)
      val gt50Set = union(positiveSet, greaterThan50Set)

      assert(contains(evenWith1, 1), "Even with 1 contains 1")
      assert(contains(evenWith1, 2), "Even with 1 contains 2")
      assert(contains(evenWith1, 8), "Even with 1 contains 8")
      assert(!contains(evenWith1, 7), "Even with 1 does not contain 7")

      assert(contains(all, 1), "all contains 1")
      assert(contains(all, 2), "all contains 2")
      assert(contains(all, 8), "all contains 8")
      assert(contains(all, 7), "all contains 7")

      assert(contains(gt50Set, 47), "gt50Set contains 47")
      assert(contains(gt50Set, 50), "gt50Set contains 50")
      assert(contains(gt50Set, 52), "gt50Set contains 52")
      assert(contains(gt50Set, 3), "gt50Set contains 3")
      assert(!contains(gt50Set, -1), "gt50Set does not contain -1")
    }
  }

  @Test def `Intersect`: Unit = {
    new TestSets {
      val oddWith1 = intersect(oddSet, s1)
      val empty = intersect(evenSet, oddSet)
      val gt50AndEven = intersect(evenSet, greaterThan50Set)

      assert(contains(oddWith1, 1), "oddWith1 contains 1")
      assert(!contains(oddWith1, 11), "oddWith1 does not contain 11")

      assert(!contains(empty, 1), "empty does not contain 1")
      assert(!contains(empty, 11), "empty does not contain 11")
      assert(!contains(empty, 8), "empty does not contain 8")

      assert(!contains(gt50AndEven, 47), "gt50AndEven does not contain 47")
      assert(!contains(gt50AndEven, 48), "gt50AndEven does not contain 48")
      assert(!contains(gt50AndEven, 51), "gt50AndEven does not contain 51")
      assert(contains(gt50AndEven, 52), "gt50AndEven contains 52")
    }
  }

  @Test def `Diff`: Unit = {
    new TestSets {
      val oddWithout1 = diff(oddSet, s1)
      val evenLower50 = diff(evenSet, greaterThan50Set)

      assert(contains(oddWithout1, 3))
      assert(contains(oddWithout1, 5))
      assert(!contains(oddWithout1, 1))

      assert(contains(evenLower50, 50))
      assert(!contains(evenLower50, 58))
      assert(contains(evenLower50, 48))
    }
  }

  @Test def `Filter`: Unit = {
    new TestSets {
      val between = filter(positiveSet, evenSet)
      assert(contains(between, 8))

      assert(contains(positiveSet, 7))
      assert(!contains(between, 7))

      assert(contains(positiveSet, 17))
      assert(!contains(between, 17))

      assert(!contains(between, -4))
    }
  }

  @Test def `Forall`: Unit = {
    new TestSets {
      val evenWith1 = union(evenSet, s1)
      assert(!forall(positiveSet, (x => x % 2 == 0)))
      assert(forall(positiveSet, (x => x > 0)))
      assert(forall(positiveSet, (x => x < 1100)))

      assert(!forall(evenWith1, (x => x % 2 == 0)))
    }
  }
  @Test def `Exists`: Unit = {
    new TestSets {
      assert(!exists(evenSet, (x => x == 1)))
      assert(exists(evenSet, (x => x == 2)))
      assert(exists(evenSet, (x => x < 50)))
    }
  }

  @Test def `Map`: Unit = {
    new TestSets {
      val odd = map(evenSet, (x => x + 1))
      assert(contains(odd,1))
      assert(!contains(odd, 2))
    }
  }
  @Test def `Failing tests in Coursera`: Unit = {
    new TestSets {

      val n = map(failingSet, (x => x - 1))
      assert(contains(n, 0))
      assert(contains(n, 2))
      assert(contains(n, 3))
      assert(contains(n, 4))
      assert(contains(n, 6))
      assert(contains(n, 999))
    }
  }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
