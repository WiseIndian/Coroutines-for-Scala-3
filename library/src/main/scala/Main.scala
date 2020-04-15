import coroutines._
import coroutines.Macros._

object Main { 
  def main(args: Array[String]): Unit = {
    // val co = coroutine[Int]({
    //   println("hello world")
    //   yieldval(2*2)
    //   yieldval(3)
    // })

    // (0 until 3) foreach (_ => println(co.continue()))




    def getIterator(ls: List[String]) = coroutine[String] {
      def foreach(f: String => Unit): Unit = {
        var cur = ls
        while (!cur.isEmpty) {
          f(cur.head)
          cur = cur.tail
        }
      }

      foreach(e => yieldval(e))
    }

    val ls = List("one","two","three","four")
    val iterator = getIterator(ls)

    var i = 0
    iterator.run { println }
  }

  

}
