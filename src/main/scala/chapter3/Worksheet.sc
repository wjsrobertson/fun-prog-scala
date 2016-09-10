sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

val list = List(1, 2, 3, 4, 5)

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
val ex1 = dropWhile(list, (x: Int) => x < 4)

def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
  case Nil => Nil
  case Cons(x, xs) => if (f(x)) Cons(x, dropWhile2(xs)(f))
  else dropWhile2(xs)(f)
}
val ex2 = dropWhile2(list)(_ < 4)

def init[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case Cons(x, Nil) => l
  case Cons(x, xs) => init(xs)
}

def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
  case Nil => z
  case Cons(x, xs) => f(x, foldRight(xs, z)(f))
}

def sum2(xs: List[Int]) = foldRight(xs, 0)(_ + _)

def product2(xs: List[Int]) = foldRight(xs, 1)(_ * _)

val test = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

def length[A](as: List[A]): Int = foldRight(as, 0)((x, y) => y + 1)

def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
  case Nil => z
  case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
}

def sum3(xs: List[Int]) = foldLeft(xs, 0)(_ + _)

def length2[A](as: List[A]): Int = foldLeft(as, 0)((x, y) => x + 1)

def reverse[A](as: List[A]) = foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))

/*
def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
  def g (a: A, b:B): B = f(b, a)
  foldRight(as, z)(g)
}

foldLeft2(list, 0)(_+_)
*/

def append[A](as: List[A])(z: A) = foldRight(as, Cons(z, Nil))((a, b) => Cons(a, b))

def concat[A](as: List[A], bs: List[A]): List[A] = as match {
  case Nil => bs
  case cs => foldRight(cs, bs)((c, b) => Cons(c, b))
}
concat(List(1, 2, 3), List(4, 5))

def flatten[A](as: List[List[A]]) = foldRight(as, Nil: List[A])((bs, cs) => concat(bs, cs))
flatten(List(List(1, 2, 3), List(4, 5)))

// Exercise 3.16
def plus1(xs: List[Int]) = foldRight(xs, Nil: List[Int])((a, b) => Cons(a + 1, b))

// Exercise 3.17
def toString(xs: List[Double]) = foldRight(xs, Nil: List[String])((a, b) => Cons(a.toString, b))
toString(List(0, 1, 2, 3))

// Exercise 3.18
def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))
map(List(1, 2, 3))(_.toString + "foo")

// Exercise 3.19
def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])(
  (a, b) => if (f(a)) Cons(a, b)
  else b
)
filter(List(1, 2, 3))(_ == 2)

// Exercise 3.20
def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])(
  (a: A, bs: List[B]) => concat(f(a), bs)
)
flatMap(List(1, 2, 3))((x: Int) => List(x + 10, x + 20))

// Exercise 3.21
def filter2[A](xs: List[A])(f: (A => Boolean)) = flatMap(xs)(
  x => if(f(x)) List(x)
       else Nil
)
filter2(List(1, 2, 3))(_ == 2)

// Exercise 3.22
def adder(as: List[Int], bs: List[Int]): List[Int] = as match {
  case Nil => Nil
  case Cons(c, cs) => bs match {
    case Nil => Nil
    case (Cons(d, ds)) => Cons(c+d, adder(cs, ds))
  }
}
adder(List(1,2,3) , List(4,5,6))

// Exercise 3.23
def zipWith[A](as: List[A], bs: List[A], f: (A, A) => A): List[A] = as match {
  case Nil => Nil
  case Cons(c, cs) => bs match {
    case Nil => Nil
    case (Cons(d, ds)) => Cons(f(c,d), zipWith(cs, ds, f))
  }
}

// Exercise 3.24
def hasSubSeq(as: List[Int], ss: List[Int]): Boolean = as match {
  case Nil => false
  case Cons(b, bs) => ss match {
    case Nil => true
    case Cons(c, cs) => if(c==b) hasSubSeq(bs, cs)
                        else hasSubSeq(bs, ss)
  }
}
hasSubSeq(List(1,2,3,4,5,6), List(3,4,5))
hasSubSeq(List(1,2,3,4,5,6), List(3,4,9))