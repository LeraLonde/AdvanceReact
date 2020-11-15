package lectures.part4implicits

trait MyTypeClassTemplate[T] {
  def action(value: T): String
}

// you can just use MyTypeClassTemplate[object]
object MyTypeClassTemplate {
  def apply[T](implicit instance: MyTypeClassTemplate[T]) = instance
}