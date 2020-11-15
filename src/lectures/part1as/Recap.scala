package lectures.part1as

import scala.annotation.tailrec

object Recap extends App {

  val aCondition: Boolean = false
  val aConditionVal = if (aCondition) 42 else 65
  // instructions vs expressions

  // compiler infers types for us
  val aCodeBlock = {
    if (aCondition) 54
    56
  }

  // Unit = void (side effects) - changing value of variable / printing to console.
  val theUnit = println("hello Scala")

  // functions
  def aFuncton(x: Int): Int = x + 1

  // recursion: stack and tail
  @tailrec def factorial(n: Int, accumulator: Int): Int =
    if (n <= 0) accumulator
    else factorial(n - 1, n * accumulator)

  // OOP
  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog // subtyping polymorphism

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("Crunch!")
  }

  // method notations
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog // natural language

  val y = 1 + 2
  val x = 1.+(2)

  // anonymous classes
  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("roar")
  }

  // generics
  abstract class MyList[+A] // variance and variance problems in THIS COURSE - covariance list
  // singletons object and companions
  object MyList

  // case classes - lightweight data structure, has a companion and all the utility functions implemented
  case class Person(name: String, age: Int)

  // exceptions and try/catch/finally exceptions
  // the type of this expression is 'Nothing' type
  val throwsException = throw new RuntimeException
  val aPotentialFailure = try {
    throw new RuntimeException
  } catch {
    case e: Exception => "I caught an exception"
  } finally {
    println("some logs")
  }

  // packaging and imports

  // functional programming - functions are instances of classes with apply method
  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }
  incrementer(1)

  // lambda - anonymous functions
  val anonymousIncrementer = (x: Int) => x + 1
  List(1,2,3).map(anonymousIncrementer) // map = Higher order function
  // map / flatmap / filter

  // for-comprehension - for all the cross pair of two lists
  // chain of map and flatmap and filters
  val pairs = for {
    num <- 1 to 3 if num % 2 == 0
    char <- List('a', 'b', 'c')
  } yield s"$num-$char"

  // Scala collections: Seqs, Arrays, Lists, Vectors, Maps, Tuples
  val aMap = Map(
    "Daniel" -> 789,
    "Jess" -> 555
  )

  // "collections": Options, Try
  val anOption = Some(2)

  // pattern matching
  val z = 2
  val order = z match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => z + "th"
  }

  // decomposition using pattern matching
  val bob = Person("Bob", age = 22)
  val greeting = bob match {
    case Person(name, _) => s"Hello! $name"
  }
}
