package lectures.part3concurrency

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success, Try}

object FuturesPromises extends App {

  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife // calcualte the meaning of life on Another thread
  } // (global) which is passed by the compiler

  println(aFuture.value) // Option[Try[Int]]

  println("Waiting on the future")

  // partial function so can be changed to this.
  aFuture.onComplete {
    case Success(meaningOfLife) => println(s"the meaning of life is $meaningOfLife")
    case Failure(e) => println(s"I have failed with $e")
  } // SOME thread - cannot assume which thread runs this

  Thread.sleep(3000)

  // mini social network
  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile) = println(s"${this.name} poking ${anotherProfile.name}")
  }

  object SocialNetwork {
    // "database" of profile
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-dummy" -> "Dummy"
    )

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )

    val random = new Random()

    // API
    def fetchProfile(id: String): Future[Profile] = Future {
      // simulating fetching from the DB
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }

  // client: mark to poke bill
  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")
  mark.onComplete {
    case Success(markProfile) => {
      val bill = SocialNetwork.fetchBestFriend(markProfile)
      bill.onComplete {
        case Success(billProfile) => markProfile poke billProfile
        case Failure(e) => e.printStackTrace()
      }
    }
    case Failure(ex) => ex.printStackTrace()
  }
  Thread.sleep(1000)

  // functional composition of futures
  // map, flatMap, filter
  val nameOnTheWall = mark.map(profile => profile.name) // from Future[Profile] => Future[String]
  val marksBestFriend = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile)) // from Future[Profile] => Future[Profile]
  val zucksBestFriendRestricted = marksBestFriend.filter(profile => profile.name.startsWith("Z"))

  // for-comprehension
  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriend(mark)
  } mark poke bill

  Thread.sleep(1000)

  // fallbacks - returns a Profile
  val aProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("fb.id.0-dummy", "Forever alone")
  }

  // returns a Futures[Profile]
  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  val fallbackResult = SocialNetwork.fetchProfile("unknown id").fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy"))

  // online bankiing app
  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Rock the JVM banking"

    def fetchUser(name: String): Future[User] = Future {
      // simulate fetching from the database
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      // simulate some processes
      Thread.sleep(1000)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      // fetch the user from the DB
      // create a transaction from the username and the merchant name
      // WAIT for the transaction to finish
      val transactionStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status

      // this method is magically injected into the int class - implicit conversions -> pimp my library
      // This call will block till the future is completed.
      Await.result(transactionStatusFuture, 2.seconds)
    }
  }
  println(BankingApp.purchase("Daniel", "IPhone 12", "rock the jvm store", 3000))

  // manual future manipulating with promises
  val promise = Promise[Int]() // "controller" over a future
  val future = promise.future // future is under the mgmt of this promise

  // thread 1 - "consumer"
  future.onComplete{
    case Success(r) => println(s"[consumer] I've received ${r}")
  }

  // thread 2 - "producer"
  val producer = new Thread(() => {
    println("[producer] crunching numbers...")
    Thread.sleep(500)
    // "fulfilling" the promise
    promise.success(42)
    println("[producer] done")
  })

  producer.start()
  Thread.sleep(1000)

  /*
    1) fulfill a future IMMEDIATELY with a value
    2) inSequence(futureA, futureB) - will run futureB after futureB is complete
    3) first(futureA, futureB) => new future (either value of futureA or futureB - whichever first)
    4) last(futureA, futureB) => new future with the last value
    5) retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T]
   */

  // exercise 5 - retryUntil
  def retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T] = {
    val future = action()
    if (!condition(future.value.get.get)) retryUntil(action, condition)
    else future
  }

  def retryUntil2[A](action: () => Future[A], condition: A => Boolean): Future[A] = {
    action()
      .filter(condition)
      .recoverWith {
        case _ => retryUntil2(action, condition)
      }
  }
  val random = new Random()
  val action = () => Future {
    Thread.sleep(100)
    val nextValue = random.nextInt(100)
    println(s"generated ${nextValue}")
    nextValue
  }

  retryUntil2(action, (x: Int) => x < 50).foreach(result => println(s"settled at ${result}"))
  Thread.sleep(10000)

  // exercise 1 - future immediately
  def fulfillImmediately[T](value: T): Future[T] = Future(value)

  // exercise 2 - inSeq
  def inSequence[A,B](futureA: Future[A], futureB: Future[B]): Future[B] = {
    futureA.flatMap(_ => futureB)
  }

  // exercise 3 - first
  def first[T](futureA: Future[T], futureB: Future[T]): Future[T] = {
    if (futureA.isCompleted) futureA
    else if (futureB.isCompleted) futureB
    else first(futureA, futureB)
  }

  def first2[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val promise = Promise[A]
    def tryComplete(promise: Promise[A], result: Try[A]) = result match {
      case Success(r) => try {
        promise.success(r)
      } catch {
        case _ =>
      }
      case Failure(t) => try {
        promise.failure(t)
      } catch {
        case _ =>
      }
    }
    fa.onComplete(promise.tryComplete)
    fb.onComplete(promise.tryComplete)
    promise.future
  }

  // exercise 4 - last
  def last[T](futureA: Future[T], futureB: Future[T]): Future[T] = {
    if (futureA.isCompleted) futureB
    else if (futureB.isCompleted) futureA
    else last(futureA, futureB)
  }

  def last2[T](fa: Future[T], fb: Future[T]): Future[T] = {
    val bothPromise = Promise[T]
    val lastPromise = Promise[T]

    val checkAndComplete = (result: Try[T]) =>
      if (!bothPromise.tryComplete(result))
        lastPromise.complete(result)

    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)
    lastPromise.future
  }

  val promiseAEx3 = Promise[Int]()
  val promiseBEx3 = Promise[Int]()
  val futureA3 = promiseAEx3.future
  val futureB3 = promiseBEx3.future
  promiseAEx3.success(10000)
  promiseBEx3.success(4)
  val returnedFuture = first(futureA3, futureB3)
  val lastFuture = last(futureA3, futureB3)
  returnedFuture.onComplete {
    case Success(r) => println(s"${r} - returned first")
  }
  lastFuture.onComplete {
    case Success(r) => println(s"${r} - returned last")
  }
}
