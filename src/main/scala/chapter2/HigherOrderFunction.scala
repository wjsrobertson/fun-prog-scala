package chapter2

import scala.annotation.tailrec

/**
  * Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function:
  *
  * def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
  */
object HigherOrderFunction {

  def main(args: Array[String]): Unit = {

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      @tailrec
      def sorted(i: Int): Boolean = {
        if (i == as.length - 1) true
        else if (!ordered(as(i), as(i + 1))) false
        else sorted(i + 1)
      }

      if (as.length <= 1) true
      else sorted(0)
    }

    println(isSorted[Int](Array(1, 2, 3, 4, 5), (x, y) => x<y))
    println(isSorted[Int](Array(1, 2, 3, 5, 4), (x, y) => x<y))
  }

}
