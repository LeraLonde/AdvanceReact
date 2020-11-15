package lectures.part1as

object AdvancePatternMatching extends App {
  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"the only element is $head")
    case _ =>
  }

  /*
    - constants
    - wildcards (e.g. _)
    - case classes
    - tuples
    - some special magic like above
   */

  // for some reason u cant make your class as a case class - but u want to make it compatible with pattern matching
  class Person(val name: String, val age: Int)

  // 1. defining a companion object
  object Person {
    // 2. define an unapply method that returns an option of a tuple
    def unapply(person: Person): Option[(String, Int)] = {
      if (person.age < 21) None
      else Some((person.name, person.age))
    }

    // you can also overload the unapply object
    def unapply(age: Int): Option[String] =
      Some(if (age < 21) "minor" else "major")
  }

  val bob = new Person("Bob", 25)
  val greeting = bob match {
    case Person(n, a) => s"Hi my name is $n and I am $a years old"
  }

  /*
    How does pattern matching work:
      Look for an object call Person
      Look for the unapply method which returns an Option of tuple
        is it empty ? - yes then it returns a match error because it returns an empty object
        no? - then
      // case Person had no relation to the class Person
      // actually you can just create a singleton object with a proper unapply method but as a practice we create it as a companion object
   */
  println(greeting)

  val legalStatus = bob.age match {
    case Person(status) => s"My legal status is $status"
  }
  println(legalStatus)

  /*
    Exercise.
   */
  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg > -10 && arg < 10
  }

  val n: Int = 24
  val mathProperty = n match {
    case singleDigit() => "a single digit"
    case even() => "an even number"
    case _ => "no property"
  }
  println(mathProperty)

  // how to write our own custom infix pattern
  case class Or[A, B](a: A, b: B) // In scala such a class is call Either
  val either = Or(2, "two")
  val humanDescription = either match {
    case number Or string => s"$number is written as $string" // compiler rewrite it to case Or(number, string)
  }
  println(humanDescription)

  // decomposing sequences
  val vararg = numbers match {
    case List(1, _*) => "starting with 1" // pattern matching against the list as a sequence
  }

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }
  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1, 2, _*) => "starting with 1, 2"
    case _ => "something else"
  }
  println(decomposed)

  // custom return types for unapply
  // isEmpty: Boolean, get: something
  // return type doesn't need to be Option but just a class with isEmpty and get
  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      def isEmpty = false
      def get = person.name
    }
  }

  println(bob match {
    case PersonWrapper(n) => s"This person's name is $n"
    case _ => "An alien"
  })
}
