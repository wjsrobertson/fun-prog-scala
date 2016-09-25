import scala.{Some => _, None => _, Option => _, Either => _, _}

trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

case object None extends Option[Nothing] {
  override def map[B](f: (Nothing) => B): Option[B] = None
  override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None
  override def filter(f: (Nothing) => Boolean): Option[Nothing] = None
  override def getOrElse[B >: Nothing](default: => B): B = default
  override def orElse[B >: Nothing](ob: Option[B]): Option[B] = ob
}

case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: (A) => B): Option[B] = Some(f(get))
  override def flatMap[B](f: (A) => Option[B]): Option[B] = f(get)
  override def filter(f: (A) => Boolean): Option[A] =
    if (f(get)) this
    else None
  override def getOrElse[B >: A](default: => B): B = get
  override def orElse[B >: A](ob: Option[B]): Option[B] = None
}

val l = List(new Some(1), new Some(2), new Some(3))
val s = Seq(1.0, 2.0, 3.0, 4.0)

def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] = {
  mean(xs) flatMap (m => mean(xs map (x => Math.pow(m - x, 2))))
}
variance(s)

def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

val abs0: Option[Double] => Option[Double] = lift(math.abs)

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch {
    case e: Exception => None
  }

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a flatMap (aa => b map (bb => f(aa, bb)))

def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
  case Nil => Some(Nil)
  case x :: xs => x flatMap (xx => sequence(xs) map (y => xx :: y))
}

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case Nil => Some(Nil)
  case x :: xs => f(x) flatMap (xx => traverse(xs)(f) map (y => xx :: y))
}

traverse(List("1", "2"))(x => Try {
  x.toInt
})

def map22[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  for {
    aa <- a
    bb <- b
  } yield f(aa, bb)