package lectures.part4implicits

import java.util.Optional
import java.{util => ju}

object ScalaJavaConversion extends App {

  // Scala List vs Java List
  import collection.JavaConverters._

  val javaSet = new ju.HashSet[Int]()
  (1 to 5).foreach(javaSet.add)
  println(javaSet)

  val scalaSet = javaSet.asScala

  import collection.mutable._
  val numbersBuffer = ArrayBuffer[Int](1, 2, 3)
  val juNumbersBuffer = numbersBuffer.asJava
  // the conversion and the references are equal in this case
  println(juNumbersBuffer.asScala eq numbersBuffer)

  val numbers = List(1,2,3) // immutable type
  val juNumbers = numbers.asJava
  val backToScala = juNumbers.asScala
  println(backToScala eq numbers)
  println(backToScala == numbers)

  // this is illegal becase scala list should be immutable
  // juNumbers.add(7)

  /*
    Exercise
    create a Scala-Java Optional to Scala Option
   */
  class LanguageConvertor[T](value: T) {
    def asScala: T = value
    def asJava: T = value
  }

  implicit def asScalaOptional[T](o: ju.Optional[T]): LanguageConvertor[Option[T]] = new LanguageConvertor[Option[T]](
    if (o.isPresent) Some(o.get) else None
  )

  implicit def asJavaOptional[T](o: Option[T]): LanguageConvertor[Optional[T]] = new LanguageConvertor[Optional[T]](
    if (o.isDefined) Optional.of(o.get) else Optional.empty
  )

  val juOptional: ju.Optional[Int] = ju.Optional.of(2)
  val scalaOption = juOptional.asScala
  println(scalaOption)
  val javaOptional = scalaOption.asJava
  println(javaOptional)
}
