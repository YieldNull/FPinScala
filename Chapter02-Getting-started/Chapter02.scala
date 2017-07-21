object Chapter02 {
  
  /* Exercise1 */
  def fib(n: Int): Int = {
    def aux(secondPre: Int, firstPre: Int, n: Int): Int = {
      if (n == 0) secondPre
      else if (n == 1) firstPre
      else aux(firstPre, secondPre + firstPre, n - 1) 
    }
    aux(0, 1, n)
  }

  /* Exercise1 BigInt */
  import scala.math.BigInt
  def fibBigInt(n: Int): BigInt = {
    def aux(secondPre: BigInt, firstPre: BigInt, n: Int): BigInt = {
      if (n == 0) secondPre
      else if (n == 1) firstPre
      else aux(firstPre, secondPre.+(firstPre), n - 1)
    }
    aux(BigInt.int2bigInt(0), BigInt.int2bigInt(1), n)
  }

  /* Exercise2 */
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    def loop(i: Int): Boolean = {
      if (i == as.length - 1) true
      else if (gt(as(i), as(i + 1))) false
      else loop(i + 1)
    }
    loop(0)
  }

  /* Exercise3 */
  def partiall[A,B,C](a: A, f: (A,B) => C): B => C =
    b => f(a, b)

  /* Exercise4 */
  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    a => b => f(a, b)

  /* Exercise5 */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = 
    (a, b) => f(a)(b)

  /* Exercise6 */
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
