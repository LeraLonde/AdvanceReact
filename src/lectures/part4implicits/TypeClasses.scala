package lectures.part4implicits

object TypeClasses extends App {
  // Ordering trait is an instance of TypeClass
  // a typeclass is an trait
  trait HTMLWritable {
    def toHtml: String
  }
  // all the classes that implement this trait will have this toHtml Method
  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHtml: String = s"<div>$name ($age yo) <a href=$email/></div>"
  }

  User("John", 32, "john@rockthejvm.com").toHtml
  /*
    2 big disadvantages
    1 - only work for types WE write - for any other types for java standard date or other library
      - we need to write come conversion for other types
    2 - ONE implementation out of quite a number. (if we say toHtml to a user when a user is logged in or not)
      - not the best design
   */

  // option 2 - pattern matching
  object HTMLSerializerPM {
    def serializeToHtml(value: Any) = value match {
      case User(n, a, e) =>
      case _ =>
    }
  }

  /*
    not the best desing still
    we gain some benefit but we lost some advantage
    1. we lost type safety because value can be anything
    2. need to modify the code every time
    3. still ONE implementation for each given type.
   */
  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }

  implicit object UserSerializer extends HTMLSerializer[User] {
    override def serialize(user: User): String =s"<div>${user.name} (${user.age} yo) <a href=${user.email}/></div>"
  }

  val john = User("John", 32, "john@rockthejvm.com")
  println(UserSerializer.serialize(john))

  // 1 - we can define serializers for other types - even types that we have not written
  import java.util.Date
  object DateSerializer extends HTMLSerializer[Date] {
    override def serialize(date: Date): String = s"<div>${date.toString}</div>"
  }

  // 2 - we can define MULTIPLE serializer for a certain type
  object PartialUserSerializer extends HTMLSerializer[User] {
    override def serialize(user: User): String = s"<div>${user.name}</div>"
  }

  // part 2
  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    def apply[T](implicit serializer: HTMLSerializer[T]) = serializer
  }
  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"<div style: color=blue>$value</div>"
  }
  println(HTMLSerializer.serialize(42))
  println(HTMLSerializer.serialize(john))

  // access to the entire type class interface
  // HTMLSerializer[User] = invokes the apply method which returns the serializer from the implicit scope.
  println(HTMLSerializer[User].serialize(john))

  // part 3
  implicit class HTMLEnrichment[T](value: T) {
    def toHTML(implicit serializer: HTMLSerializer[T]): String = serializer.serialize(value)
  }
  // because user class does not have the toHTML method so it look for an implicit class which has that and wrap the object around it.
  println(john.toHTML(UserSerializer)) // println(new HTMLEnrichment[User](john).toHTML(UserSerializer))
  println(john.toHTML) // because UserSerializer was declared implicit - and it takes in an implicit serializer
  // because HTMLEnrichment can wrap around any type.
  println(2.toHTML)
  // we can choose implementation - by importing the serializer to the local scope or declare it explicitly
  println(john.toHTML(PartialUserSerializer))

  /*
    - type class itself HTMLSerializer[T] { ... }
    - type class instances (some of which are implicit) UserSerializer, IntSerializer
    - conversion with implicit classes HTMLEnrichment
   */

  // context bounds
  def htmlBoilerplate[T](content: T)(implicit serializer: HTMLSerializer[T]): String =
    s"<html><body>${content.toHTML(serializer)}</body></html>"

  // this context bound [T : HTMLSerializer] is telling the compiler to ONLY HTMLSerializer of type T
  // but we cannot use the serializer by name because the compiler would injects it for us
  def htmlSugar[T : HTMLSerializer](content: T): String = {
    val serializer = implicitly[HTMLSerializer[T]]
    // use the serializer by name
    s"<html><body>${content.toHTML(serializer)}</body></html>"
  }

  // implicitly - is a method
  case class Permissions(mask: String)
  implicit val defaultPermissions = Permissions("0744")

  // in some other part of the code - we want to surface out what is the implicit value of the permission
  val standardPerms = implicitly[Permissions]
}
