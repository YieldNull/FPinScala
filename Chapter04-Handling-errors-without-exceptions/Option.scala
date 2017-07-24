import scala.{Option => _, Either => _, _}

/* Exercise 1 */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(a) => a
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case None => None
      case Some(a) => f(a)
    }

  def flatMap1[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None
  
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case _  => this 
    }

  def orElse1[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(a) if f(a) => Some(a)
      case _ => None
    }

  def filter1(f: A => Boolean): Option[A] =
    flatMap(a => if(f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  
  /* Exercise 2 */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean (xs.map(x => math.pow(x - m , 2))))
  

  import java.util.regex._
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  /* Exercise 3 */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  /* Exercise 4 */
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))((m1, m2) => m1(s) && m2(s))

  /* Exercise 5 */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case x :: xs => x flatMap (xx => sequence(xs) map (xx :: _))
    }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    //a.foldRight[Option[List[A]]](Some(Nil))((o, acc) => acc flatMap (cc => o map ( _ :: cc)))
    //a.foldRight[Option[List[A]]](Some(Nil))((o, acc) => o flatMap (oo => acc map (oo :: _)))
    a.foldRight[Option[List[A]]](Some(Nil))((o, acc) => map2(o, acc)(_ :: _))
  
  /* Exercise 6 */
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, acc) => map2(f(x), acc)(_ :: _))

  def sequenceViaTransfer[A](a: List[Option[A]]): Option[List[A]]=
    traverse(a)(x => x)
}
