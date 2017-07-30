trait RNG {
  def nextInt: (Int, RNG)
}


object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
                  ((1L << 48) - 1)

      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  /* Exercise 1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, rng2) = rng.nextInt
    (if(v < 0) -(v + 1) else v, rng2)
  }

  /* Exercise 2 */
  def double(rng: RNG): (Double, RNG) = {
    val (v, rng2) = nonNegativeInt(rng)

    (v / (Int.MaxValue.toDouble + 1), rng2)
  }

  /* Exercise 3 */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  /* Exercise 4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if(count > 0) {
      val (i, rng2) = rng.nextInt
      val (l, rngn) = ints(count - 1)(rng2)
      (i :: l, rngn)
    } else (Nil, rng)

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def aux(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) =
      if(count > 0) {
        val (i, rng2) = rng.nextInt
        aux(count - 1, rng2, i :: acc)
      } else {
        (acc, rng)
      }
    aux(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)
  
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (v, rng2) = s(rng)
      (f(v), rng2)
    }
  
  /* Exercise 5 */
  def positiveMax(n: Int): Rand[Int] =
    rng => {
      val (v, rng2) = nonNegativeInt(rng)
      (v % (n + 1), rng2)
    }

  /* Exercise 6 */
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt(_))((i => i /(Int.MaxValue.toDouble + 1)))

  /* Exercise 7 */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rnd => {
      val (a, rnd2) = ra(rnd)
      val (b, rnd3) = rb(rnd2)
      (f(a, b), rnd3)
    }

  def intDoubleViaMap2(rng: RNG): ((Int, Double), RNG) =
    map2(int, double)((_, _))(rng)

  def doubleIntViaMap2(rng: RNG): ((Double, Int), RNG) =
    map2(double, int)((_, _))(rng)

  /* Exercise 8 */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      val (xs, rrng) = fs.foldLeft((List[A](), rng))((acc, r) => {
        val (l, rng2) = acc
        val (v, rng3) = r(rng2)
        (v :: l, rng3)
      })

      (xs.reverse, rrng)
    }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))(map2(_, _)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence2(List.fill(count)(int))

  /* Exercise 9 */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (v, rng2) = f(rng)
      g(v)(rng2)
    }

  def positiveInt: Rand[Int] =
    flatMap(int)(i => if(i != Int.MinValue) unit(i.abs) else positiveInt)

  /* Exercise 10 */
  def mapViaFlatMap[A,B](f: Rand[A])(g: (A => B)): Rand[B] =
    flatMap(f)(a => unit(g(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

case class State[S,+A](run: S => (A,S)) {
  import State._
 
  /* Exercise 11 */
  def flatMap[B](f: (A => State[S,B])): State[S,B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })

  def map[B](f: (A => B)): State[S,B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](s: State[S,B])(f: (A, B) => C): State[S,C] =
    flatMap(a => s.map(b => f(a, b)))
}

object State {
  def unit[S,A](a: A): State[S,A] =
    State((s => (a, s)))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit[S,List[A]](List()))((a, acc) =>(a map2 acc)(_ :: _))

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def modify2[S](f: S => S): State[S, Unit] =
    get flatMap (s => set(f(s)) map (a => ()))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  import State._
  
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)
    }
  
  def simulateMachine(inputs: List[Input]): State[Machine, Int] = 
    for {
      _ <- sequence(inputs map ( (modify[Machine] _) compose update))
      s <- get
    } yield s.coins
}
