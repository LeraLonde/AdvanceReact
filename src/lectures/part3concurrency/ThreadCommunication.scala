package lectures.part3concurrency

import scala.collection.mutable
import scala.util.Random

object ThreadCommunication extends App {

  /*
    the producer-consumer problem
    A single container wrapping a single value [ x ]
    producer -> [ ? ] -> consumer
    (set)                (get)
   */
  class SimpleContainer {
    private var value: Int = 0

    def isEmpty: Boolean = value == 0

    def set(newValue: Int) = value = newValue

    def get = {
      val result = value
      value = 0
      result
    }
  }

  def naiveProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      while (container.isEmpty) {
        println("[Consumer] actively waiting...")
      }
      println(s"[consumer] I have consumed ${container.get}");
    })

    val producer = new Thread(() => {
      println("[Producer] computing...")
      Thread.sleep(500)
      val value = 42
      println(s"[producer] I have produced, after long work, the value ${value}")
      container.set(value)
    })

    consumer.start()
    producer.start()
  }

  //naiveProdCons()

  /*
    evaluating a synchronized expression on an object locks the object
    lock the object's monitor - any other thread trying to run this will block
    once it is done - then it will release the block.

    Only AnyRefs can have synchronized block - and not primitive types
    General Principles:
    1. make no assumption that who gets the lock first
    2. keep locking to a minimum
    3. maintain thread safety at ALL times in parallel application
   */

  // wait and notify
  /*
    waiting on an object's monitor suspends your calling thread indefinitely
    when you call wait() - it will release the lock and wait.

    thread2 - notify() - will signal ONE sleeping thread they may continue - but u donno which thread

    waiting and notifying only work in synchronized expressions
   */

  def smartProdCons() = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      container.synchronized {
        container.wait()
      }

      // at this point container must have some value
      // the only 1 that can wake up the consumer is the producer thread.
      println(s"[consumer] I have consumed ${container.get}")
    })

    val producer = new Thread(() => {
      println("[producer] Hard at work...")
      Thread.sleep(2000)
      val value = 42

      container.synchronized {
        println(s"[producer] I am producing ${value}")
        container.set(value)
        container.notify()
      }
    })
    consumer.start()
    producer.start()
  }

  //smartProdCons()

  /*
    producer -> [ ? ? ? ] -> consumer
    producer produce multiple values and consumer will consume.
    1. if the buffer is full then the producer need to block
    2. if the consumer is too fast then the consumer need to consume more
   */

  def prodConsLargeBuffer(): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3

    val consumer = new Thread(() => {
      val random = new Random()
      while (true) {
        // synchronize on the buffer
        buffer.synchronized {
          if (buffer.isEmpty) {
            println("[consumer] buffer empty, waiting..")
            // waiting on the buffer object's monitor
            buffer.wait()
          }
          // there must be at least ONE value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer] consumed ${x}")

          // hey producer, there's empty space available
          buffer.notify()
        }
        Thread.sleep(random.nextInt(250))
      }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0

      while (true) {
        buffer.synchronized {
          if (buffer.size == capacity) {
            println("[producer] buffer is full, waiting...")
            buffer.wait()
          }
          // there must be at least ONE EMPTY SPACE in the buffer for me to produce the value
          println(s"[producer] producing ${i}")
          buffer.enqueue(i)

          // hey consumer, new food for you!
          buffer.notify()
          i += 1
        }
        Thread.sleep(random.nextInt(500))
      }
    })

    consumer.start()
    producer.start()
  }

  //prodConsLargeBuffer()

  /*
    prod-cons, lvl 3

    (multiple) producer --> [ ? ? ? ] --> (multiple) consumer
   */
  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      while (true) {
        // synchronize on the buffer
        buffer.synchronized {
          while (buffer.isEmpty) {
            println(s"[consumer ${id}] buffer empty, waiting..")
            // waiting on the buffer object's monitor
            buffer.wait()
          }
          // there must be at least ONE value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer ${id}] consumed ${x}")

          // signify somebody to wake up be it consumer or producer
          buffer.notify()
        }
        Thread.sleep(random.nextInt(250))
      }
    }
  }

  class Producer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      var i = 0

      while (true) {
        buffer.synchronized {
          while (buffer.size == capacity) {
            println(s"[producer ${id}] buffer is full, waiting...")
            buffer.wait()
          }
          // there must be at least ONE EMPTY SPACE in the buffer for me to produce the value
          println(s"[producer ${id}] producing ${i}")
          buffer.enqueue(i)
          // notify somebody to wake up be it consumer or producer
          buffer.notify()
          i += 1
        }
        Thread.sleep(random.nextInt(500))
      }
    }
  }

  def multiProdCons(nConsumers: Int, nProducers: Int): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 20

    (1 to nConsumers).foreach(i => new Consumer(i, buffer). start())
    (1 to nProducers).foreach(i => new Producer(i, buffer, capacity).start())
  }

  //multiProdCons(3, 6)

  /*
    Exercises.
    1) think of an example where notifyAll acts in a different way than notify?
    2) create a deadlock - when one thread block each other and they cannot continue
    3) create a livelock - when one thread yield execution to each other but they cannot continue.
   */

  // notifyall
  def testNotifyAll(): Unit = {
    val bell = new Object

    (1 to 10).foreach(i => new Thread(() => {
      bell.synchronized {
        println(s"[thread ${i}] waiting..")
        bell.wait()
        println(s"[thread ${i} hooray]")
      }
    }).start())

    new Thread(() => {
      Thread.sleep(2000)
      println("[announcer] Rock n Roll")
      bell.synchronized {
        bell.notify()
      }
    }).start()
  }

  // notifyAll - wake every one up
  // notify - causes a deadlock when everyone is waiting for someone to wait them up
  // testNotifyAll()

  // 2 - deadlock - bowing example - you wait for the person to rise before u rise
  case class Friend(name: String) {
    def bow(other: Friend) = {
      this.synchronized {
        println(s"$this: I am bowing to my friend $other")
        other.rise(this)
        println(s"$this: my friend $other has risen")
      }
    }

    def rise(other: Friend) = {
      this.synchronized {
        println(s"$this: I am rising to my friend $other")
      }
    }

    var side = "right"
    def switchSide(): Unit = {
      if (side == "right") side = "left"
      else side = "right"
    }

    def pass(other: Friend): Unit = {
      while (this.side == other.side) {
        println(s"$this: Oh, but please, $other, feel free to pass..")
        switchSide()
        Thread.sleep(1000)
      }
    }
  }

  val sam = Friend("Sam")
  val pierre = Friend("Pierre")

  // new Thread(() => sam.bow(pierre)).start() // sam's lock, then pierre's lock
  // new Thread(() => pierre.bow(sam)).start() // pierre's lock, then sam's lock

  // 3 - livelock
  new Thread(() => sam.pass(pierre)).start()
  new Thread(() => pierre.pass(sam)).start()
}
