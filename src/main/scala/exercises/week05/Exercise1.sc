

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

init(List(1,2,3))
init(List(1,2,3,4,5,6))
init(List(1))

List(1).tail
//List().init

def removeAt[T](n: Int, xs: List[T]): List[T] = {
  def it(i: Int, ys: List[T], xs: List[T]): List[T] = {
    if (xs.isEmpty) ys
    else if (i-1 == n) it(i - 1, ys, xs.init)
    else it(i - 1, xs.last :: ys, xs.init)
  }
  it(xs.length, List(), xs)
}

def removeAtSolved[T](n: Int, xs: List[T]): List[T] = xs.take(n) ::: xs.drop(n + 1)

removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)
removeAtSolved(1, List('a', 'b', 'c', 'd')) // List(a, c, d)


def flatten(xs: List[Any]): List[Any] = {
  def iter(ys: List[Any], xs: List[Any]): List[Any] = {
    if (xs.isEmpty) ys
    else if (xs.last.isInstanceOf[List[_]]) iter(flatten(xs.last.asInstanceOf[List[Any]]) ::: ys, xs.init)
    else iter(xs.last :: ys , xs.init)
  }
  iter(List[Any](), xs)
}

flatten(List(List(1, 1), 2, List(3, List(5, 8)))) // List[Any] = List(1, 1, 2, 3, 5, 8)
