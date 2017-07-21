sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]):Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0 // short-circuiting
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
 
  /* Exercise1 */
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  } // x: Int = 3

  /* Exercise2 */
  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => throw new UnsupportedOperationException("tail of empty list")
    case Cons(x,xs) => xs
  }

  /* Exercise3 */
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  /* Exercise4 */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs)(f)
      case _ => l
    }

  /* Exercise5 */
  def setHead[A](l: List[A], x: A): List[A] =
    l match {
      case Nil => throw new UnsupportedOperationException("setHead on empty list")
      case Cons(_, tail) => Cons(x, tail)
    }

  /* Exercise6 */
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new UnsupportedOperationException("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  /* Exercise7
   * No. Before we call f(the outer one), we evaluate all its arguments,
   * which means tranversing the list all the way to the end.
   */

  /* Exercise8
   * We get back the origin list.
   *
   * List(1,2,3) = Cons(1, Cons(2, Cons(3, Nil)))
   */

  /* Exercise9 */
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => 1 + acc)

  /* Exercise10 */
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  /* Exercise11 */
  def sum3(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => 1 + acc)

  /* Exercise12 */
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, x) => Cons(x, acc))

  /* Exercise13 */
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((x, acc) => f(acc, x))
  
  /* Exercise14 */
  def append[A](la: List[A], lb: List[A]) =
    foldRight(la, lb)(Cons(_, _))

  /* Exercise15 */
  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil:List[A])(append)

  /* Exercise16 */
  def add1(l: List[Int]): List[Int] = 
    foldRight(l, Nil:List[Int])((x, acc) => Cons(x + 1, acc))
  
  /* Exercise17 */
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((x, acc) => Cons(x.toString, acc))

  /* Exercise18 */
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((x, acc) => Cons(f(x), acc))

  /* Exercise19 */
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((x, acc) => if(f(x)) Cons(x, acc) else acc)

  /* Exercise20 */
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil:List[B])((x, acc) => append(f(x), acc))

  /* Exercise21 */
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if(f(x)) List(x) else Nil)

  /* Exercise22 */
  def addPair(la: List[Int], lb: List[Int]): List[Int] =
    (la, lb) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPair(xs, ys))
    }

  /* Exercise23 */
  def zipWith[A,B,C](la: List[A], lb: List[B])(f: (A, B) => C): List[C] =
    (la, lb) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs,ys)(f))
    }

  /* Exercise24 */
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def aux[A](ll: List[A], subb: List[A]): Boolean =
      (ll, subb) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(x, xs), Cons(y, ys)) =>
          if(x == y) aux(xs, ys)
          else aux(xs, sub)
      }
    aux(l, sub)
  }
}
