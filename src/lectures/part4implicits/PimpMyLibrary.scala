package lectures.part4implicits

object PimpMyLibrary extends App {

  // Enrichment gives us the ability to enrich library that you do not have access to
  // decorate existing classes that we have no access to with additional methods and properties

  // 2.isPrime for example - it can only take one and only parameter
  // for optimization purpose implicit classes always extends AnyVal
  // but the parameter need to be a val
  implicit class RichInt(val value: Int) extends AnyVal {
    def isEven: Boolean = value % 2 == 0
    def sqrt: Double = Math.sqrt(value)
    def times(func: () => Unit): Unit = {
      def timesAux(n: Int): Unit =
        if (n <= 0) ()
        else {
          func()
          timesAux(n - 1)
        }

      timesAux(value)
    }
    def *[T](list: List[T]): List[T] = {
      def ListConcatenator(list: List[T], count: Int): List[T] = {
        if (count == 1) list
        else list ++ ListConcatenator(list, count - 1)
      }
      ListConcatenator(list, value)
    }
  }

  implicit class RicherInt(richInt: RichInt) {
    def isOdd: Boolean = richInt.value % 2 != 0
  }

  new RichInt(42).sqrt
  // we added isEven into the Integer class - this is call type enrichment = pimping
  42.isEven // new RichInt(42).isEven <- compiler will rewrite this
  1 to 10
  println(3 * List(1, 2))

  import scala.concurrent.duration._

  3.seconds

  // compiler doesn't do multiple implicit searches.
  // 42.isOdd -- this will not work as compiler only does it once and would not apply two layers of implicit

  /*
    Enrich the String class
    - asInt
    - encrypt
      "John" -> Lnjp

     keep enriching the Int class
     - times(function)
       3.times(() => ...)
     - *
       3 * List(1,2) => List(1,2,1,2,1,2)
   */

  implicit class enrichString(val value: String) extends AnyVal {
    def asInt: Int = Integer.parseInt(value)

    def encrypt(cypherDistance: Int): String = {
      // string.map(x = > (x + cypherDistance).asInstanceOf[Char])
      value.toList.map(x => (x + cypherDistance).toChar).mkString
    }
  }

  println("3".asInt + 4)
  println("John".encrypt(2))

  // auto convert string to integer - "3" / 4
  implicit def stringToInt(string: String): Int = Integer.parseInt(string)

  println("6" / 2) // stringToInt("6") / 2

  // these two lines is the same as writing - implicit class RichAltInt(value: Int)
  // implicit conversion are powerful but discouraged
  class RichAltInt(value: Int)
  implicit def enrich(value: Int): RichAltInt = new RichAltInt(value)

  // danger zone
  implicit  def intToBoolean(i: Int): Boolean = i == 1
  /*
    if (n) do something
    else do something else
   */

  val aConditionedValue = if (3) "OK" else "Something wrong"
  println(aConditionedValue)

  /*
    Best practice
    - keep type enrichment to implicit classes and type classes
    - avoid implicit defs as much as possible
    - package implicits clearly, bring into scope only what you need
    - IF you need conversions, make them specific
   */
}
