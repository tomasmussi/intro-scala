

val xs = Array(1,2,3,4)
xs map (x => x * 2)

// s is a sequence thus it is an iterable.
val s = "Hello World"

s filter (c => c.isUpper)
s exists (c => c.isUpper)
s forall (c => c.isUpper)

val l = List(1,2,3)
val z = l zip s

z.unzip

s flatMap(c => List('.', c))

xs.sum

xs.max

1 to 20 map(x => (1 to 10 map (y => (x, y))))

def scalarProduct(xs : Vector[Double], ys: Vector[Double]): Double = {
  (xs zip ys).map(xy => xy._1 * xy._2).sum
}
def scalarProductMatch(xs : Vector[Double], ys: Vector[Double]): Double = {
  (xs zip ys).map{case (x, y) => x * y}.sum
  /**
   * Simplifies x => match {case p1 => e1, case p2 => e2...}
   * to { case p1 => e1, case p2 => e2... }
   * */
}

def isPrime(n: Int): Boolean = {
  //(2 to (n - 1)).map(x => n % x != 0).forall(x => x)
  //(2 until n).map(x => n % x != 0).forall(x => x)
  (2 until n).forall(x => n % x != 0)
}


isPrime(4)
isPrime(17)
isPrime(301)
isPrime(307)

def whichPrime(n: Int) = {
  (2 until n).map(x => (x, n % x != 0)).filter( (x => !x._2) )
}

whichPrime(301)
7 * 43
whichPrime(307)