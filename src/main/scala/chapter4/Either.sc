
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

  def orElse[EE >: E, B >: A](o: Either[EE, B]): Either[EE, B]

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: (Nothing) => B): Either[E, B] = Left(value)

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = Left(value)

  override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] = this

  override def orElse[EE >: E, B >: Nothing](o: Either[EE, B]): Either[EE, B] = o
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: (A) => B): Either[Nothing, B] = Right(f(value))

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    b map (bb => f(value, bb))
  }

  override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] = f(value)

  override def orElse[EE >: Nothing, B >: A](o: Either[EE, B]): Either[EE, B] = Right(value)
}

def mean(xs: IndexedSeq[Double]): Either[String, Double] =
  if (xs.isEmpty) Left("mean of empty list")
  else Right(xs.sum / xs.length)

def Try[A](a: => A): Either[Exception, A] =
  try Right(a)
  catch {
    case e: Exception => Left(e)
  }

def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
  case Nil => Right(Nil)
  case x :: xs => for {
    xx <- x
    yy <- sequence(xs)
  } yield xx :: yy
}

def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
  case Nil => Right(Nil)
  case x :: xs => f(x) flatMap (xx => traverse(xs)(f) map (yy => xx :: yy))
}

def traverse2[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
  case Nil => Right(Nil)
  case x :: xs => for {
    xx <- f(x)
    yy <- traverse2(xs)(f)
  } yield xx :: yy
}