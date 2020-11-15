package exercises

import lectures.part4implicits.TypeClasses.{User}


object EqualityPlayground extends App {
  trait Equal[T] {
    def compare(valueA: T, valueB: T): Boolean
  }

  object UserSharesTheSameEmail extends Equal[User] {
    override def compare(userA: User, userB: User): Boolean = userA.email equals userB.email
  }

  implicit object UserSharesTheSameName extends Equal[User] {
    override def compare(userA: User, userB: User): Boolean = userA.name equals userB.name
  }

  object Equal {
    def compare[T](valueA: T, valueB: T)(implicit comparator: Equal[T]): Boolean =
      comparator.compare(valueA, valueB)

    def apply[T](implicit comparator: Equal[T]) = comparator
  }

  val john = User("John", 32, "john@rockthejvm.com")
  val anotherJohn = User("John", 45, "anotherJohn@rtjvm.com")
  println(Equal.compare(john, anotherJohn))

  // AD-HOC polymorphism - depends on the actual type the compiler decides which typeclass it should use to compare it.

  /*
    Exercise - let's improve Equal typeclass with an implicit conversion class.
    method:
    - === (anotherValue: T)
    - !== (anotherValue: T)
   */
  implicit class TypeSafeEqual[T](value: T) {
    def ===(anotherValue: T)(implicit comparator: Equal[T]): Boolean = comparator.compare(value, anotherValue)
    def !==(anotherValue: T)(implicit comparator: Equal[T]): Boolean = !comparator.compare(value, anotherValue)
  }
  println(john === anotherJohn)
  // john.===(anotherJohn) but user does not have a === method
  // compiler will look for a implicit class with an === and convert it to the below.
  // new TypeSafeEqual(john).===(anotherJohn)
  // But the method also takes an implicit Equal[User] type class - and the implicit Equal[User] is UserSharesTheSameName
  // new new TypeSafeEqual(john).===(anotherJohn)(UserSharesTheSameName)
  println(john !== anotherJohn) // new TypeSafeEqual(john).!==(anotherJohn)

  /*
    TYPE SAFE
   */
  //println(john == 43) - you can compare anything
  //println(john === 43) // compiler will prevent u from compiling
}
