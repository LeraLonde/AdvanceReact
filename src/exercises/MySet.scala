package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {
  /*
    EXERCISE - implement a functional set
   */
  def apply(elem: A): Boolean =
    contains(elem)

  def contains(elem: A): Boolean

  def +(elem: A): MySet[A]

  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B]

  def filter(predicate: A => Boolean): MySet[A]

  def foreach(f: A => Unit)

  def -(elem: A): MySet[A]

  def &(anotherSet: MySet[A]): MySet[A] // intersection
  def --(anotherSet: MySet[A]): MySet[A] // difference
  def unary_! : MySet[A]

  /*
    EXERCISE
    - removing an element
    - intersection with another set
    - difference with another set
   */
}

class EmptySet[A] extends MySet[A] {
  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def contains(elem: A): Boolean = false

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def foreach(f: A => Unit): Unit = ()

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]

  override def -(elem: A): MySet[A] = this

  override def &(anotherSet: MySet[A]): MySet[A] = this

  override def --(anotherSet: MySet[A]): MySet[A] = this

  // set[1,2,3] => infinite number of element
  // be careful and make sure we leave a space between the : and !
  override def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

// all elements of type A which satisfy a property
// { x in A | property(x) }
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  def contains(elem: A): Boolean = property(elem)

  // { x in A | property(x) } + element = { x in A | property(x) || x == elem }
  def +(elem: A): MySet[A] = {
    new PropertyBasedSet[A](x => property(x) || x == elem)
  }

  // { x in A | property(x) } ++ set => { x in A | property(x) || set contains x }
  def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  // all intergers => (_ % 3) => [0 1 2]
  def map[B](f: A => B): MySet[B] = politelyFail

  def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  def foreach(f: A => Unit) = politelyFail

  def filter(predicate: A => Boolean): MySet[A] =
    new PropertyBasedSet[A](x => property(x) && predicate(x))

  def -(elem: A): MySet[A] = filter(x => x != elem)

  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")
}

class NonEmptySet[A](val head: A, val tail: MySet[A]) extends MySet[A] {
  override def +(elem: A): MySet[A] = {
    if (this contains elem) this
    else new NonEmptySet[A](elem, this)
  }

  /*
    [1,2,3] ++ [4, 5]
     = [2 3] ++ [4, 5] + 1
     = [3] ++ [4, 5] + 1 + 2
     = [] ++ [4, 5] + 1 + 2 + 3
     = [4, 5] + 1 + 2 + 3 = [4, 5, 1, 2, 3]
   */
  override def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head

  override def contains(elem: A): Boolean =
    elem == head || tail.contains(elem)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }

  override def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  override def map[B](f: A => B): MySet[B] = (tail map f) + f(head)

  override def -(elem: A): MySet[A] =
    if (head == elem) tail
    else tail - elem + head

  /*
    break down
    1. filter(x => anotherSet.contains(x))
    2. as contains and apply is essentially the same so you can rewrite it to filter(x => anotherSet(x))
    3. applying sugar -- filter(anotherSet)
   */
  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet) // intersection = filtering !

  override def --(anotherSet: MySet[A]): MySet[A] = filter(x => !anotherSet(x))

  // new operator
  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))
}

object MySet {
  // var A* : I can provide multiple value of type A here
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] = {
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    }

    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayground extends App {
  val s = MySet(1, 2, 3, 4)
  s + 5 ++ MySet(-1, -2) + 3 flatMap (x => MySet(x, 10 * x)) filter (_ % 2 == 0) foreach println

  val negative = !s // s.unary_! = all the naturals not equal to 1,2,3,4
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(x => x % 2 == 0)
  println(negativeEven(5))
  println(negativeEven(6))

  val negativeEven5 = negativeEven + 5
  println(negativeEven5(5))
}