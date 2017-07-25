
trait Stream[+A] {
  /* Exercise 1 */
  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(hd, tl) => hd() :: tl().toList
    }

  import Stream._

  /* Exercise 2 */
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }
  }
  
  /* Exercise 3 */
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Empty => z
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
    }
  
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, acc) => p(a) || acc)
  
  /* Exercise 4 */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => p(a) && acc)
  
  /* Exercise 5 */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) =>
        if(p(a)) cons(a, acc)
        else empty[A])
  
  /* Exercise 6 */
  def map[B](f: (A => B)): Stream[B] =
    foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(p: (A => Boolean)): Stream[A] =
    foldRight(empty[A])((a, acc) => if(p(a)) cons(a, acc) else acc)

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((a, acc) => cons(a, acc))

  def flatMap[B](f: (A => Stream[B])): Stream[B] =
    foldRight(empty[B])((a, acc) => f(a) append acc)

  /* Exercise 12 */
  def mapViaUnfold[B](f: (A => B)) =
    unfold(this) {
      case Cons(h, tl) => Some(f(h()), tl())
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((n, this)) {
      case (1, Cons(h, _)) => Some(h(), (0, empty))
      case (i, Cons(h, tl)) if i > 1 => Some(h(), (i - 1, tl()))
      case _ => None
    }

  def takeWhileViaUnfold(p: (A => Boolean)): Stream[A] =
    unfold(this) {
      case Cons(h, tl) if p(h()) => Some(h(), tl())
      case _ => None
    }

  def zipWithViaUnfold[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zip[B](s: Stream[B]): Stream[(A,B)] =
    zipWithViaUnfold(s)((_, _))

  def zipAllWithViaUnfold[B,C](s: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) => Some(f(None, Some(h2())), (empty, t2()))
      case (Cons(h1, t1), Empty) => Some(f(Some(h1()), None), (t1(), empty))
      case (_, _) => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    zipAllWithViaUnfold(s)((_, _))

  /* Exercise 14 */
  def tails: Stream[Stream[A]] =
    unfold[Stream[A],Option[Stream[A]]](Some(this)) {
      case Some(Cons(h, tl)) => Some(cons(h(), tl()), Some(tl()))
      case Some(Empty) => Some(empty, None)
      case _ => None
    }

  def tails2: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case Cons(h ,tl) => Some(cons(h(), tl()), tl()) 
    } append Stream(empty)
  
  /* Exercise 15 */
  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, acc) => {
      lazy val acc_ = acc // ensure only one evaluation
      val acc2 = f(a, acc_._1)
      (acc2, cons(acc2, acc_._2))
    })._2
}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))

  /* Exercise 7 */
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))
  
  /* Exercise 8 */
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  /* Exercise 9 */
  def fibs: Stream[Int] = {
    def aux(snd: Int, fst: Int): Stream[Int] =
      cons(snd, aux(fst, snd + fst))
    aux(0, 1)
  }

  /* Exercise 10 */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Empty
      case Some((a, s)) => cons(a, unfold(s)(f)) 
    }

  /* Exercise 11 */
  def onesViaUnfold(): Stream[Int] =
    unfold(1)(_ => Some(1, 1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a,a))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(i => Some(i+1,i+1))

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (snd, fst) => Some(snd, (fst, snd + fst)) }

  /* Exercise 13 */
  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean =
    s.zipAllWithViaUnfold(s2) {
      case (Some(x), Some(y)) => x == y
      case (_, None) => true
      case (None, _) => false
    }.foldRight(true)((_ && _))

  def hasSubsequences[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails exists (startsWith(_, s2))
}
