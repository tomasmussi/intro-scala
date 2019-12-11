

def squareListMatch(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y * y :: squareListMatch(ys)
  }

/* Using map */
def squareList(xs: List[Int]): List[Int] =
  xs map (x => x * x)

val l = List(1,2, -1,3,4)

squareListMatch(l)
squareList(l)

l filter(x => x % 2==0)
l filterNot(x => x % 2==0)
l partition(x => x % 2 == 0)
l takeWhile(x => x > 0)
l dropWhile(x => x > 0)
l span(x => x > 0)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  //case x :: xs1 => (xs takeWhile(y => x == y)) :: pack(xs dropWhile(y => x == y))
  case x :: xs1 =>
    val (same, diff) = xs span (y => y == x)
    same :: pack(diff)
}

pack(List("a", "a", "a", "b", "c", "c", "a"))
//List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs).map(l => (l.head, l.length))
}

encode(List("a", "a", "a", "b", "c", "c", "a"))
//List(("a", 3), ("b", 1), ("c", 2), ("a", 1))