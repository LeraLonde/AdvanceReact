package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App {
  /*
    interface Runnable {
      public void run()
    }
    interface = traits on scala
   */
  // JVM threads
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Running in parallel")
  })

  // create a JVM thread => runs on top of OS thread
  aThread.start() // gives the signal to the JVM to start a JVM thread
  aThread.join() // Make sure thread is completed before running - blocks until aThread finishes running

  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))
  threadHello.start()
  threadGoodbye.start()
  // different runs produce different results!

  // executors - Threads are expensive to start and kill - so solution is to reuse them
  val pool = Executors.newFixedThreadPool(10)
  pool.execute(() => println("Something in the thread pool"))
  pool.execute(() => {
    Thread.sleep(1000)
    println("done after 1 second")
  })
  pool.execute(() => {
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(1000)
    println("done after two seconds")
  })

  pool.shutdown()
  // no more action can be submitted
  // pool.execute(() => println("should not appear")) // throws an exception in the calling thread

  // interrupt all the sleeping thread that is running in the pool
  // pool.shutdownNow()
  println(pool.isShutdown) // true = even if action submitted is still running

  def runInParallel = {
    var x = 0

    val thread1 = new Thread(() => x = 1)
    val thread2 = new Thread(() => x = 2)
    thread1.start()
    thread2.start()

    println(x)
  }

  /*for (_ <- 1 to 100000) runInParallel*/
  // race condition - 2 thread setting a single memory zone

  class BankAccount(@volatile var amount: Int) {
    override def toString: String = s"$amount"
  }

  def buy(account: BankAccount, thing: String, price: Int) = {
    account.amount -= price
    //println(s"I've bought $thing")
    //println(s"My account is now $account")
  }

/*  for (_ <- 1 to 1000) {
    val account = new BankAccount(50000)
    val thread1 = new Thread(() => buy(account, "shoes", 3000))
    val thread2 = new Thread(() => buy(account, "iphone12", 4000))

    thread1.start()
    thread2.start()
    Thread.sleep(10)
    if (account.amount != 43000) println("AHA : " + account)
  }*/

  // option #1: use synchronized()
  def buySafe(account: BankAccount, thing: String, price: Int) =
    account.synchronized {
      // no two threads can evaluate this at the same time
      account.amount -= price
      //println(s"I've bought $thing")
      //println(s"My account is now $account")
    }

  for (_ <- 1 to 10) {
    val account = new BankAccount(50000)
    val thread1 = new Thread(() => buySafe(account, "shoes", 3000))
    val thread2 = new Thread(() => buySafe(account, "iphone12", 4000))

    thread1.start()
    thread2.start()
    Thread.sleep(10)
    //if (account.amount != 43000) println("AHA : " + account)
  }

  // option #2: use @volatile

  /*
    1) Construct 50 "inception" threads
        Thread1 -> thread2 -> thread3 -> ...
        println("hello from thread #3")

       print thread in reverse order
   */
  def recursiveThread(count: Int): Unit = {
    if (count != 0) {
      new Thread(() => {
        println(s"Hello from #$count")
        recursiveThread(count - 1)
      }).start()
    }
  }
  recursiveThread(5000)

  /*
    2
   */
  var x = 0
  var threads = (1 to 100).map(_ => new Thread(() => x += 1))
  threads.foreach(_.start())
  /*
    1) what is the biggest value possible for x? 100
    2) what is the smallest value possible for x? 1
   */

  /*
    3 sleep fallacy
   */
  var message = ""
  val awesomeThread = new Thread(() => {
    Thread.sleep(1000)
    message = "Scala is awesome"
  })
  message = "Scala sucks"
  awesomeThread.start()
  Thread.sleep(2000)
  awesomeThread.join() // wait for the awesome thread to join <-- wait for the awesomeThread to finish.
  println(message)
  /*
    what's the value of message? almost always "Scala is awesome"
    is it guaranteed? No!
    why or why not?

   (main thread)
    message = "Scala sucks"
    awesomeThread.start()
      sleep() - relieves execution
    (awesome thread)
      sleep() - relieves execution
    (OS gives the CPU to some important thread - takes CPU for more than 2 seconds)
    (OS gives the CPU back to the MAIN thread)
      println("Scala sucks")
    (OS gives the CPU to awesomethread)
      message = "Scala is awesome"

    synchronizing only works if two seperate thread trying to edit at the same time.
   */
}
