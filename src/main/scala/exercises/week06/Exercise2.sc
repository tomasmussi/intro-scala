

val n = 7
def isPrime(n: Int): Boolean = {
  (2 until n).forall(x => n % x != 0)
}

/**
 * Look for pairs (i, j) where j < i, and i + j is a prime number
 * */
(1 until n).flatMap(i => (1 until i).map(j => (i,j))).filter(pair => isPrime(pair._1 + pair._2))

/**
 * With generators it is clearer
 * */

for {
  i <- 1 until n
  j <- 1 until i
  if (isPrime(i + j))
} yield (i,j)

def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
  (for ((i, j) <- xs zip ys) yield i * j).sum
}
scalarProduct(List(1,0,2), List(4,3,2))