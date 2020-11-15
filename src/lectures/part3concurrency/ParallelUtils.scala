package lectures.part3concurrency

object ParallelUtils extends App {

  def measure[T](operation: => T): Long = {
    val time = System.currentTimeMillis()
    operation
    System.currentTimeMillis() - time
  }

  val list = (1 to 1000000).toList
  val serialTime = measure {
    list.map(_ + 1)
  }
}
