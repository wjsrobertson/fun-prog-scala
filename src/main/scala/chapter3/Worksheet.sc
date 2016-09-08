sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

val l = List(1, 2, 3, 4, 5)

def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case Cons(x, xs) => x + sum(xs)
}

def product(ds: List[Double]): Double = ds match {
  case Nil => 1.0
  case Cons(0.0, _) => 0.0
  case Cons(x, xs) => x * product(xs)
}

def tail[A](xs: List[A]): List[A] = xs match {
  case Nil => Nil
  case Cons(x, ys) => ys
}

def setHead[A](x: A, xs: List[A]): List[A] = xs match {
  case Nil => Cons(x, Nil)
  case Cons(y, ys) => Cons(x, ys)
}

def drop[A](xs: List[A], n: Int): List[A] =
  if (n == 0) xs
  else drop(tail(xs), n - 1)

def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  case Nil => Nil
  case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f))
  else dropWhile(xs, f)
}

def init[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case Cons(x, Nil) => l
  case Cons(x, xs) => init(xs)
}

init(l)