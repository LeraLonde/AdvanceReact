package lectures.part4implicits

object OrganizingImplicits extends App {

  // this will take precedences and override the default ordering in scala.predef
  // implicit val normalOrdering: Ordering[Int] = Ordering.fromLessThan(_ < _)
  implicit val reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  // sorted method takes in an implicit method for ordering if left out
  // implicit can be found within scala.Predef which is included before
  println(List(1,4,5,3,2).sorted)

  // potential implicit values are:
  /*
    - val/var
    - object
    - accessor methods = defs with no parameter (no parenthesis) - if there is a parenthesis it will not be considered
   */

  // Exercise
  case class Person(name: String, age: Int)

  val persons = List(
    Person("steve", 30),
    Person("Amy", 22),
    Person("John", 66)
  )

  // if you put it as a part of a companion object then it will find it
  /*object Person {
    implicit val orderByName: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  }*/
  // then this implicit will take precedence over the one that was defined within the companion object.
  /*implicit val ageOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.age < b.age)*/

  // No implciit ordering defined for person
  // println(persons.sorted)

  // it does not look for all the methods within an object - so compiler will complain
  /*object SomeObject {
    implicit val orderByName: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  }*/

  /*
    Implicit scope
    - normal scope has the highest priority = LOCAL SCOPE
    - imported scope = futures example which import executorContext global
    - companions of all types involved in the method signature
      - it will look for a implicit ordering in List
      - Ordering companion object
      - all the types involved = A or any supertype
   */

  /*
    Best practices
      when you want to define an implicit val:
      #1 - if there is a single possible value for it
         - and you can edit the code for the type
      then define the implicit value in the companion object of that type.

      #2 - if there is are many possible implicit value for it
         - but a single good one
         - and you can edit the code for the type
      then define the good implicit in the companion while the others in the local scope
   */

  // if age and alphabet will happen (package them in individual object) - then we will package it within a special object
  object AlphabeticNameOrdering {
    implicit val orderByName: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  }
  object AgeOrdering {
    implicit val ageOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.age < b.age)
  }
  import AgeOrdering._
  println(persons.sorted)

  /*
    Exercise.
      - ordering by totalPrice = most used (50%) used
      - ordering by unit count = 25% used
      - ordering by unit price = 25% used
   */
  case class Purchase(nUnits: Int, unitPrice: Double)
  object Purchase {
    implicit val orderByTotalPrice: Ordering[Purchase] = Ordering.fromLessThan((a, b) => {
      val totalPriceA = a.nUnits * a.unitPrice
      val totalPriceB = b.nUnits * b.unitPrice
      totalPriceA < totalPriceB
    })
  }
  object OrderByUnitCount {
    implicit val orderByUnitCount: Ordering[Purchase] = Ordering.fromLessThan(_.nUnits < _.nUnits)
  }
  object OrderByUnitPrice {
    implicit val orderByUnitPrice: Ordering[Purchase] = Ordering.fromLessThan(_.unitPrice < _.unitPrice)
  }
}
