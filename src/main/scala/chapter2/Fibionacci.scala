import scala.annotation.tailrec

/*
Exercise 2.1
TODO - make me a test
 */
object Fibionacci {
  def main(args: Array[String]): Unit = {

    def fib(n: Int) = {
      @tailrec
      def fibHelper(f1: Int, f2: Int, t: Int): Int = {
        if (n == t) f1 + f2
        else fibHelper(f2, f1+f2, t+1)
      }

      if (n == 1) 0
      else if (n == 2) 1
      else fibHelper(0, 1, 3)
    }

    println(fib(1) + "," + fib(2) + "," + fib(3) + "," + fib(4) + "," + fib(5) + "," + fib(6) + "," + fib(7))
  }

  // 1 2 3 4 5 6 7
  // 0 1 1 2 3 5 8
}