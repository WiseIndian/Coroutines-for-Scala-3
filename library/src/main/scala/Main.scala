import coroutines.Macros._
import coroutines._


class A 
class B extends A
object Main { 
  def main(args: Array[String]): Unit = { 
    val co2 = coroutine[B] {}

    val co1 = coroutine[B] {
      join(co2)
    }

  }
}