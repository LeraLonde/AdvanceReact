package exercises

import scala.annotation.tailrec

object StreamsPlayground extends App {
  val naturals = MyStream.from(1)(x => x + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFrom0 = 0 #:: naturals // naturals.#::(0)
  println(startFrom0.head)

  println(startFrom0.take(10000).foreach(println))

  // map, flatMap
  println(startFrom0.map(_ * 2).take(100).toList())
  println(startFrom0.flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))).take(10).toList())

  // filtering on an infinite stream will no guarantee to give u a finite stream
  println(startFrom0.take(11).filter(_ < 10).toList())

  /*
    [first, [....]]
    [first, fibo(second, first+second)]
   */
  def fibonacci(first: BigInt, second: BigInt): MyStream[BigInt] =
    new Cons(first, fibonacci(second, first + second))

  println(fibonacci(1, 1).take(100).toList())

  // Exercises on streams
  // 1 - stream of fibonacci numbers
  // 2 - stream of prime numbers with Eratosthenes' sieve
  /*
    [2, 3, 4 ....]
    filter out all numbers divisible by 2
    [2, 3, 5, 7, 9, 11 ...]
    filter out all numbers divisible by 3
    [2, 3, 5, 7, 11, ...]
    filter out all numbers divisble by 5
     ...
   */
  def eratosthenes(numbers: MyStream[Int]): MyStream[Int] =
    if (numbers.isEmpty) numbers
    else new Cons(numbers.head, eratosthenes(numbers.tail.filter(_ % numbers.head != 0)))

  println(eratosthenes(MyStream.from(2)(_ + 1)).take(100).toList())
  val fibonacci = MyStream.from((0, 1))(tup => (tup._2, tup._1 + tup._2)).map(_._1).takeAsList(10)
  println(fibonacci)
}

/*
  Exercise: implement a lazily evaluated, singly linked STREAM of elements.
  head is always available - the tail is always lazy
  naturals = MyStream.from(1)(x => x + 1) = stream of natural numbers (potentially infinite!)
  naturals.take(100).foreach(println) // lazily evaluated stream of the first 100 naturals (finite stream)
  naturals.foreach(println) // will crash
  naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
*/
object MyStream {
  // Generator will generate the next value base on the start value
  def from[A](start: A)(generator: A => A): MyStream[A] = {
    new Cons(start, MyStream.from(generator(start))(generator))
  }
}

abstract class MyStream[+A] {
  def isEmpty: Boolean

  def head: A

  def tail: MyStream[A]

  // prepend a element to the stream
  def #::[B >: A](element: B): MyStream[B] // prepend operator
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] // concatenate

  def foreach(f: A => Unit): Unit

  def map[B](f: A => B): MyStream[B]

  def flatMap[B](f: A => MyStream[B]): MyStream[B]

  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A] // takes the first n elements out of the stream
  def takeAsList(n: Int): List[A] = take(n).toList()

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] = {
    if (isEmpty) acc.reverse
    else tail.toList(head :: acc)
  }
}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException

  def tail: MyStream[Nothing] = throw new NoSuchElementException

  // prepend a element to the stream
  def #::[B >: Nothing](element: B): MyStream[B] = new Cons(element, this) // prepend operator
  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream // concatenate

  def foreach(f: Nothing => Unit): Unit = ()

  def map[B](f: Nothing => B): MyStream[B] = this

  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  def take(n: Int): MyStream[Nothing] = this // takes the first n elements out of the stream
}

class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false

  override val head: A = hd
  // combining call by name parameter + lazy val = call by need
  override lazy val tail: MyStream[A] = tl

  // prepend a element to the stream
  def #::[B >: A](element: B): MyStream[B] = new Cons(element, this) // prepend operator
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons(head, tail ++ anotherStream) // concatenate

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  /*
    s = new Cons(1, ?)
    mapped = s.map(_ + 1) = new Cons(2, s.tail.map(_ + 1) )
    .... ** not evaluated unless i use mapped.tail in some later expression
   */
  def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail map f) // preserves lazy evaluation
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

  def filter(predicate: A => Boolean): MyStream[A] = {
    if (predicate(head)) new Cons(head, tail.filter(predicate))
    else tail.filter(predicate) // only the first element on the tail will be evaluated but not the rest of the tail.
  }

  def take(n: Int): MyStream[A] = {
    if (n <= 0) EmptyStream
    else if (n == 1) new Cons(head, EmptyStream)
    else new Cons(head, tail.take(n - 1))
  } // takes the first n elements out of the stream
}

