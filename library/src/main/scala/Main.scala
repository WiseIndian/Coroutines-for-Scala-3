import coroutines._
import coroutines.Macros._

object Main { 
  def main(args: Array[String]): Unit = {
    val co = coroutine[Int]({
      println("hello world")
      yieldval(2*2)
      yieldval(3)
    })

    (0 until 3) foreach (_ => println(co.continue()))
  }

}
