package lectures.part2afp

/*
  what is a monad ? - a list of elements
  - Monad is an abstract type which have some fundamental ops
  - unit operation (apply) or (pure) - construct a monad out of a value
  - flatMap operation (bind) - transform a certain type into a monad type parameter

  List / Option / Try / Future/ Stream / Set are all monads
  Operations must satisfy the monad laws:
  - left-identity - unit(x).flatMap == f(x)
  - right-identity - aMonadInstance.flatMap(unit) == aMonadInstance
  - associativity - m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

  Example Prove that List is monad
  left identity - satisfy
  unit = apply in this case
  List(x).flatMap(f) =
  f(x) ++ Nil.flatMap(f)
  = f(x)

  right identity
  list.flatMap(x => List(x)) = list

  associativity:
  [a b c].flatMap(f).flatMap(g) =
  (f(a) ++ f(b) ++ f(c)).flatMap(g) =
  f(a).flatMap(g) ++ f(b).flatMap(g) ++ f(c).flatMap(g) =
  [a b c].flatMap(f(_).flatMap(g)) =
  [a b c].flatMap(x => f(x).flatMap(g))

  Example Prove that Option is monad
  left identity
  Option(x).flatMap(f) = f(x)
  Some(x).flatMap(f) = f(x)

  right identity
  opt.flatMap(x => Option(x)) = opt
  Some(v).flatMap(x => Option(x)) = Some(v)

  associativity:
  o.flatMap(f).flatMap(g) = o.flatMap(x => f(x).flatMap(g))
  Some(v).flatMap(f).flatMap(g) = f(v).flatMap(g)
  Some(v).flatMap(x => f(x).flatMap(g)) = f(v).flatMap(g)
 */
object Monads extends App {

  // our own Try Monad
  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt {
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  /*
    left-identity
    unit.flatMap(f) = f(x)
    Attempt(x).flatMap(f) = f(x)
    Success(x).flatMap(f) = f(x) // proved if x throws exception it will return fail(x) which is also an attempt
   */

  /*
    right-identity
    attempt.flatMap(unit) = attempt
    Success(x).flatMap(x => Attempt(x)) = Attempt(x) = Success(x)
    Fail(_).flatMap(...) = Fail(e)
   */

  /*
    associativity = attempt.flatMap(f).flatMap(g) == attempt.flatMap(x => f(x).flatMap(g))
    Fail(e).flatMap(f).flatMap(g) = Fail(e)
    Fail(e).flatMap(x => f(x).flatMap(g)) = Fail(e)

    Success(v).flatMap(f).flatMap(g) =
      f(v).flatMap(g) OR Fail(e)

    Success(v).flatMap(x => f(x).flatMap(g)) =
      f(v).flatMap(g) OR Fail(e)
   */

  val attempt = Attempt {
    throw new RuntimeException("My own Monad")
  }
  println(attempt)

  /*
    1 - EXERCISE: implement a Lazy[T] monad = computation which will only be executed when it's needed.

    unit/apply - in a companion object
    flatMap

    2 - Monads = unit + flatMap
        Monads = unit + map + flatten
   */

  // 1 - lazy monad
  class Lazy[+A](value: => A) {
    // call by need - so that it will only evaluate once and cache the result
    private lazy val internalValue = value
    def use: A = internalValue
    // (=> A) - To delay the compiler from evaluating the value within f(value)
    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internalValue)
  }
  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

  val lazyInstance = Lazy {
    println("Today I don't feel like doing anything")
    42
  }
  val flatMappedInstance = lazyInstance.flatMap(x => Lazy { 10 * x })
  val flatMappedInstance2 = lazyInstance.flatMap(x => Lazy { 10 * x })
  flatMappedInstance.use
  flatMappedInstance2.use

  /*
    left identity -
    unit.flatMap(f) = f(v)
    Lazy(v).flatMap(f) = f(v)

    right-identity
    l.flatMap(unit) = l
    Lazy(v).flatMap(x => Lazy(x)) = Lazy(v)

    associativity law: l.flatMap(f).flatMap(g) = l.flatMap(x => f(x).flatMap(g))
    Lazy(v).flatMap(f).flatMap(g) = f(v).flatMap(g)
    Lazy(v).flatMap(x => f(x).flatMap(g)) = f(v).flatMap(g)
  */

  // 2: map and flatten in terms of flatMap
  /*
    Monad[T] {
      def flatMap[B](f: T => Monad[B]): Monad[B] = ... (implemented)
      def map[B](f: T => B): Monad[B] = flatMap(x => Monad(f(x))) // Monad[B]
      def flatten(m: Monad[Monad[T]]): Monad[T] = m.flatMap((x: Monad[T]) => x)
    }
   */
}
