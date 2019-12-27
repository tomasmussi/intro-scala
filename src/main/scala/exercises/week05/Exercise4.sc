
def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys) (_ :: _)
  //(xs foldLeft ys) (_ :: _) // Error in parameter types

concat(List(1,2,3), List(4,5,6))


def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( f(_) :: _) // First '_' is t, the second '_' is u
  //(xs foldRight List[U]())( (t, u) => f(t) :: u)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (t, i) => i + 1 )

mapFun[Int, Int](List(1,2,3,4), (x => x * x))
lengthFun[Int](List(1,2,3,4,251,1,25))