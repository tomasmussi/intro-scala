package exercises.week02

object HigherOrderUnderstanding {


  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0 else f(a) + sumF(a + 1, b)
    sumF
  }


  /**
   * Es equivalente a la funcion sum, pero esto es mas syntactic sugar.
   * Esta definicion es lo que permite hacer Scala, y asi no tener necesidad de definir la funcion
   * sumF en el medio, pero son lo mismo.
   * */
  def sumPro(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0 else f(a) + sum(f)(a + 1, b)
  }
  /**
   * Cual es el tipo de sumPro????
   * El tipo de sumPro es:
   * Una funcion que toma un Int y devuelve un Int:  (Int => Int) =>
   * para luego pararselo a otra funcion anonima que toma dos Int y devuelve un Int (Int, Int) => Int
   *
   *  (x => x * x) => (1, 10) => suma de los cuadrados de los numeros del 1 al 10
   * (Int => Int) => (Int, Int) => Int
   * */

  def main(args: Array[String]): Unit = {
    println("Suma forma 1: " + sum(x => x * x)(1, 3))
    println("Suma forma 2: " + sumPro(x => x * x)(1, 3))
    val n1 = sum(x => x * x)
    //val n2 = sumPro (x => x * x) // Asi no es valido
    val n2 = sumPro (x => x * x) _

    println(n1(1, 3))
    println(n2(1,3))
  }

}
