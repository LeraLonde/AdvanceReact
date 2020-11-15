package lectures.part4implicits

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object MagnetPattern extends App {

  // Magnet problem is solving some of the problem created by method overloading
  class P2PRequest

  class P2PResponse

  class Serializer[T]

  trait Actor {
    def receive(statusCode: Int): Int

    def receive(request: P2PRequest): Int

    def receive(response: P2PResponse): Int

    // with context bound
    // def receive[T](message: T)(implicit serializer: Serializer[T])
    def receive[T: Serializer](message: T): Int

    def receive[T: Serializer](message: T, statusCode: Int): Int

    def receive(future: Future[P2PRequest]): Int

    // This will not compile because they are the same
    // def receive(future: Future[P2PResponse]): Int
    // lots of overloads
  }

  /*
    1 - type erasure
    2 - lifting doesnt work for all overloads
      val receiveFV = receive _ // Compiler will not be able to differentiate it
    3 - code duplication
    4 - type inferernce and default args
      actor.receive(?!) - compiler doesnt know what default args to fetch
   */

  trait MessageMagnet[Result] {
    def apply(): Result
  }

  def receive[R](magnet: MessageMagnet[R]): R = magnet()

  implicit class FromP2PRequest(request: P2PRequest) extends MessageMagnet[Int] {
    def apply(): Int = {
      // logic for handling a P2PRequest
      println("Handing P2P Request")
      42
    }
  }

  implicit class FromP2PResponse(response: P2PResponse) extends MessageMagnet[Int] {
    def apply(): Int = {
      // logic for handling a P2PResponse
      println("Handing P2P Response")
      24
    }
  }

  receive(new P2PRequest)
  receive(new P2PResponse)

  /*
    Magnet patterns - benefit
    1 - no more type erasure problems!
   */
  implicit class FromResponseFuture(future: Future[P2PResponse]) extends MessageMagnet[Int] {
    override def apply(): Int = 2
  }

  implicit class FromRequestFuture(future: Future[P2PRequest]) extends MessageMagnet[Int] {
    override def apply(): Int = 3
  }

  println(receive(Future {
    new P2PRequest
  }))

  println(receive(Future {
    new P2PResponse
  }))

  // 2 - lifting work
  trait MathLib {
    def add1(x: Int): Int = x + 1
    def add1(s: String): Int = s.toInt + 1
    // add1 overloads
  }

  // "magnetize" - it works with a catch - we didnt specify the generic type if not compiler will not know what type
  // so you need to specify the concrete return type.
  trait AddMagnet {
    def apply(): Int
  }

  def add1(magnet: AddMagnet): Int = magnet()

  implicit class AddInt(x: Int) extends AddMagnet {
    override def apply(): Int = x + 1
  }

  implicit class AddString(x: String) extends AddMagnet {
    override def apply(): Int = x.toInt + 1
  }

  val addFV = add1 _
  println(addFV(1))
  println(addFV("3"))

  /*
    Drawbacks
    - super verbose
    - harder to read
    - you can't name or place default argument
    - call by name doesn't work correctly
   */

  class Handler {
    def handler(s: => String) = {
      println(s)
      println(s)
    }
  }

  trait HandleMagnet {
    def apply(): Unit
  }

  def handle(magnet: HandleMagnet) = magnet()
  implicit class StringHandle(s: => String) extends HandleMagnet {
    override def apply(): Unit = {
      println(s)
      println(s)
    }
  }

  def sideEffectMethod(): String = {
    println("Hello, Scala")
    "hahaha"
  }

  handle(sideEffectMethod())

  // so println is only called once - and hahaha is the only string that got implicitly converted
  // logging might not appear and it is hard to trace
  handle {
    println("Hello, Scala")
    "hahaha"
  }
}
