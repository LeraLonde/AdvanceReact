package lectures.part2afp

object CurriesPAF extends App {

  // curried functions
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3) // Int => Int = y => 3 + y
  println(add3(5))
  println(superAdder(3)(5)) // curried functions - receive multiple parameter list

  // curried method
  // METHOD! when you call a method u need to supply all the parameters
  def curriedAdder(x: Int)(y: Int): Int = x + y

  // if we remove the type annotation - it will not worked
  // if you remove the type - then compiler is able to convert a method into a function
  val add4: Int => Int = curriedAdder(4)
  // lifting = transforming method to a function (ETA-EXPANSION)

  // functions != methods (JVM limitations)
  def inc(x: Int): Int = x + 1

  // compile does ETA-expansion and turns method inc into a function - then use the function value on x
  // at run time - converted into List(1,2,3).map(x => inc(x))
  List(1, 2, 3).map(inc)

  // Partial function application - how to force compiler to do ETA expansion whenever you want
  val add5 = curriedAdder(5) _ // with the underscore in the end - is telling compiler to convert the method into a function

  // EXERCISE
  val simpleAddFunction = (x: Int, y: Int) => x + y

  def simpleAddMethod(x: Int, y: Int) = x + y

  def curriedAddMethod(x: Int)(y: Int) = x + y

  // add7: Int => Int = y => 7 + y
  // as many as different implementations of add7 using the above
  val add7 = curriedAddMethod(7) _
  val add7_1 = simpleAddFunction(7, _: Int) // alternative syntax for turning methods into function values
  val add7_2 = simpleAddMethod(7, _: Int) // compiler rewrite it with y => simpleAddMethod(7, y)
  val add7_3 = (x: Int) => simpleAddFunction(7, x) // simplest
  val add7_4 = simpleAddFunction.curried(7)
  val add7_5 = curriedAddMethod(7)(_) // PAF = alternative syntax for turning method to function values

  /*
    funtion and method can be applied using underscores
   */

  // underscores are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c

  // This is a function value - after you provide a concatenator then it will give u a value.
  // x: String => concatenator(hello, x, how are you)
  val insertName = concatenator("Hello, I'm ", _: String, ", how are you?")
  println(insertName("Daniel"))

  val fillInTheBlanks = concatenator("Hello, ", _: String, _: String) // ETA-expanded = (x,y) => concatenator("hello", x, y)
  println(fillInTheBlanks("Daniel", " Scala is awesome"))

  // EXERCISES
  /*
    1. Process a list of numbers and return their string representations with different formats
       Use the %4.2f, %8.6g and %14.12f with a curried formatter function
   */
  println("%4.2f".format(Math.PI))
  println("%8.6f".format(Math.PI))

  def formatter(format: String, value: Double): String = format.format(value)

  val formatBy42 = formatter("%4.2f", _: Double) // by provindg an underscore with a type - you can reduce function arity (reduce function parameter)
  val formatBy86 = formatter("%8.6g", _: Double)
  val formatBy1412 = formatter("%14.12f", _: Double)

  println(formatBy42(Math.PI))
  println(formatBy86(Math.PI))
  println(formatBy1412(Math.PI))

  def curriedFormatter(s: String)(number: Double): String = s.format(number)
  val numbers = List(Math.PI, Math.E)
  val simpleFormat = curriedFormatter("%4.2f") _ // lifting
  val seriousFormat = curriedFormatter("%8.6f") _
  val preciseFormat = curriedFormatter("%14.12f") _
  println(numbers.map(curriedFormatter("%4.2f"))) // compiler is able to do sweet ETA expansion

  /*
    2. Difference between
       - functions vs methods
       - parameters by-name vs 0-lambda
   */
  def byName(n: => Int): Int = n + 1

  def byFunction(f: () => Int): Int = f() + 1

  def method: Int = 42

  def parenMethod(): Int = 42

  val testLambda: Int => Int = x => x + 5
  val test = testLambda(5)
  /*
    calling byName and byFunction
    - int
    - method
    - parenMethod
    - lambda
    - PAF
   */
  byName(23) // ok
  byName(method) // method will be evaluated to its call
  byName(parenMethod()) // ok
  byName(parenMethod) // ok but beware ==> byName(parenMethod())
  // byName(() => 42) // not ok
  byName((() => 42)()) // ok - supply a function and calling the function to get a result
  // byName(parenMethod _) // a function value is not a substitude for a by name parameter

  // byFunction(45) - not ok
  // byFunction(method) - not ok!! - parameterless method like accessor are different from a method with a parameter - compiler does not do ETA expansion
  byFunction(parenMethod) -
  byFunction(() => 45)
  byFunction(parenMethod _) // also works, but warning - unnecessary

  println(byName(method))

  /*
    By-name parameters are evaluated every time they are used. They won’t be evaluated at all if they are unused.
    This is similar to replacing the by-name parameters with the passed expressions.
    They are in contrast to by-value parameters. To make a parameter called by-name, simply prepend => to its type.
   */

}
