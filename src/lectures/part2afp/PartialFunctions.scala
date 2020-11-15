package lectures.part2afp

object PartialFunctions extends App {
  // Function1[Int, Int] or Int => Int
  // Any int can be passed in and get the result
  val aFunction = (x: Int) => x + 1

  // sometimes you want to restrict what can be passed to the function.
  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new FunctionNotApplicableException

  class FunctionNotApplicableException extends RuntimeException

  // Anything outside will throw a match error
  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }

  // {1,2,5} => Int
  // Domain of the function is (1,2,5) => Int
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  } // partial function value - same result as aNicerFussyFunction

  println(aPartialFunction(2))
  // println(aPartialFunction(5500)) --> this would return a match error as it is done the same way

  // Partial Function utilities
  println(aPartialFunction.isDefinedAt(67)) // figure out if a partial function would accept the value

  // lift - would convert your partial function from Int => Int to Int => Option[Int]
  val lifted = aPartialFunction.lift // Int => Option[Int]
  println(lifted(2))
  println(lifted(51020))

  // orElse take another partial function as argument
  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }
  println(pfChain(2))
  println(pfChain(45)) // return 67 - from the fall back partial function specified on orElse

  // Partial function extends normal functions
  // Partial function is a subtype of total function
  // hence you can assign a partial function to a normal function
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  // HOFs accept partial functions as well
  val aMapped = List(1,2,3).map {
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }
  println(aMapped)

  /*
    Note: PF can only have ONE parameter type
   */

  /*
    Exercises
    1. - construct a PF instance yourself (anonymous class)
    2. - implement a dumb chatbot as a Partial function
   */

  // defining a manual partial function - with syntax sugar u dont need to write the boilerplate
  val aManualFussyFunction = new PartialFunction[Int, Int] {
    override def apply(x: Int): Int = x match {
      case 1 => 42
      case 2 => 65
      case 5 => 999
    }
    override def isDefinedAt(x: Int): Boolean =
      x == 1 || x == 2 || x == 5
  }

  val chatBot: PartialFunction[String, String] = {
    case "a" => "Hello World"
    case "b" => "dlrow olleh"
  }

  scala.io.Source.stdin.getLines().map(chatBot).foreach(println)
}
