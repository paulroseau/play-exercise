package utils

/*
 * Basic typeclass for type that can be combined by some inner associative law
 * */

sealed trait Summable[T] {
  def plus(t1: T, t2: T): T
}

object Summable {

  def apply[T](implicit instance: Summable[T]): Summable[T] = instance

  implicit val doubleSummable = new Summable[Double] {
    def plus(t1: Double, t2: Double): Double = t1 + t2
  }

  // For elegance
  implicit class SummableOps[T : Summable](t: T) {
    def plus(t2: T): T = Summable[T].plus(t, t2)
  }
}
