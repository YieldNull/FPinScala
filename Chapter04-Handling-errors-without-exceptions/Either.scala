
trait Either[+E, +A] {
 
  /* Exercise 7 */
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(v) => Right(f(v))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(v) => f(v)
      case Left(e) => Left(e)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(v) => b
      case Left(e) => Left(e)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (aa => b map (bb => f(aa, bb)))
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  
  /* Exercise 8 */
  def traverse[E,A,B](l: List[A])(f: (A => Either[E,B])): Either[E,List[B]] =
    l.foldRight[Either[E,List[B]]](Right(Nil))((x, acc) => f(x).map2(acc)(_ :: _))

  def sequence[E,A](l: List[Either[E,A]]): Either[E, List[A]] =
    traverse(l)(x => x)
}

