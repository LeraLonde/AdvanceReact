package lectures.part1as

import scala.util.Try

object DarkSugars extends App {

  // syntax sugar #1 : methods with single param
  def singleArgMethod(args: Int): String = s"$args little ducks..."

  // write a code block to supply the single parameter
  val description = singleArgMethod {
    // write some complex code
    42
  }

  val aTryInstance = Try { // java's try { ... }
    throw new RuntimeException
  }

  List(1, 2, 3).map { x =>
    x + 1
  }

  // syntax sugar #2: single abstract method pattern
  trait Action {
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  // compiler does alot of magic to allow lambda to become a single abstract type conversion
  val aFunkyInstance: Action = (x: Int) => x + 1

  // example : Runnables
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Hello, Scala")
  })
  val aSweeterThread = new Thread(() => println("Sweet, Scala!"))

  // As long as there exist just ONE unimplemented class then u can use single abstract method pattern
  abstract class AnAbstractType {
    def implemented: Int = 23

    def f(a: Int): Unit
  }

  val anAbstractInstance: AnAbstractType = (a: Int) => println("sweet")

  // syntax sugar #3: the :: and #:: methods are special
  val prependedList = 2 :: List(3, 4)
  // 2.::(List(3,4)) - there is no :: method on Int
  // compiler will rewrite it as List(3,4).::(2)

  // scala specification: last char decides associativity of method
  // if it ends with a : means it is right associative
  // if it is not then it is left associative
  1 :: 2 :: 3 :: List(4, 5)
  List(4, 5).::(3).::(2).::(1)

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this
  }
  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  // syntax sugar #4 : multi-word method naming
  class TeenGirl(name: String) {
    def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
  }

  val lily = new TeenGirl("Lily")
  lily `and then said` "Scala is so sweet!"

  // syntax sugar #5: infix types on Generic
  class Composite[A, B]
  val composite: Int Composite String = ???

  class --> [A, B]
  val towards: Int --> String = ???

  // syntax sugar #6 update() is very special, much like apply()
  val anArray = Array(1,2,3)
  anArray(2) = 7 // rewritten to anArray.update(idx = 2, value = 7)
  // used in mutable collections
  // remember apply() AND update()

  // syntax sugar #7: setters for mutable containers
  class Mutable {
    private var internalMember: Int = 0 // private for OO encapsulation
    def member = internalMember // getter
    // named with _=
    def member_=(value: Int): Unit = internalMember = value
  }

  val aMutableContainer = new Mutable
  // This only works if you define a getter and a setter (with a suffix of _=)
  aMutableContainer.member = 42 // rewritten as aMutableContainer.member_=(42)
}
