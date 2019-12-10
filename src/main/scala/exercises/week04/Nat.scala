package exercises.week04

import java.util.NoSuchElementException

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new Error("0.predecessor")

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if (that.isZero) this else throw new Error("negative")
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat = new Succ(n + this)

  override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}

object Main {
  def main(args: Array[String]): Unit = {
    val one = new Succ(Zero)
    val two = new Succ(one)

    val three = one + two

    println(two.successor == three)
  }
}