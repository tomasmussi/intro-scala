def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = {
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }
    }
    val (fst, snd) = xs.splitAt(n)
    merge(msort(fst), msort(snd))
  }
}


msort(List(2,1,2,6,8,3,7,5,6))
import scala.math.Ordering


def paramMsort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = {
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }
    }
    val (fst, snd) = xs.splitAt(n)
    merge(paramMsort(fst), paramMsort(snd))
  }
}

paramMsort(List(2,1,2,6,8,3,7,5,6))(Ordering.Int)
paramMsort(List(2,1,2,6,8,3,7,5,6))
paramMsort(List(2,1,2,6,8,3,7,5,6))(Ordering.Int.reverse)
