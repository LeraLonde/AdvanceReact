package lectures.part4implicits

object ImplicitIntro extends App {
  // how does this ever compile ? because string class does not have '->' class
  // this is a method of an implicit class
  val pair = "Daniel" -> "555"
  val intPair = 1 -> 2

  case class Person(name: String) {
    def greet = s"Hi, my name is $name"
  }

  implicit def fromStringToPerson(str: String): Person = Person(str)

  // magic - a string class calling method greet which is a person class
  // how does this work ? - compiler will look for all the implicit method/class which can help it get an object of person
  // compiler convert this code into: println(fromStringToPerson("Peter").greet)
  // compiler will assume that there will ONLY one implicit that matches
  println("Peter".greet)

  class A {
    def greet: Int = 2
  }

  // compiler will find that two implicit matches so that the compiler will not compile.
  // because A also has a greet method.
  // implicit def fromStringToA(str: String): A = new A

  // implicit parameters
  def increment(x: Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount = 10

  // you can also do increment(2)(3) but if left out the compiler will find an implicit argument within the search scope.
  increment(2)
}
