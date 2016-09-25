case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, T: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): List[A] = {
    def takeAcc(c: Int, s: Stream[A]): List[A] = s match {
      case Empty => Nil
      case Cons(h, t) => if (c == n) Nil
                         else h() :: takeAcc(c + 1, t())
    }

    if (n == 0) Nil
    else takeAcc(0, this)
  }

  def drop(n: Int): Stream[A] = {
    def dropAcc(c: Int, s: Stream[A]): Stream[A] = s match {
      case Empty => Empty
      case Cons(h, t) => if (c == n) t()
                         else dropAcc(c + 1, t())
    }

    if (n == 0) this
    else dropAcc(1, this)
  }

  def takeWhile(p: A => Boolean): List[A] = {
    def takeWhileAcc(s: Stream[A]): List[A] = s match {
      case Empty => Nil
      case Cons(h, t) => if (!p(h())) Nil
                         else h() :: takeWhileAcc(t())
    }

    takeWhileAcc(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
}

object Stream {
  def cons[A](hd: A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

Stream(1, 2, 3, 4, 5).take(1).toList
Stream(1, 2, 3, 4, 5).drop(1).toList
Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList
