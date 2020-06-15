import org.junit.Assert._
import coroutines._
import scala.language.implicitConversions
import scala.collection._

def assertYieldvals[T](expectedVals: List[T], co: Coroutine[T], debug: Boolean = false): Unit = {
  expectedVals.foreach { expected => 
    assert(!co.isDone())
    val yielded = co.continue()
    if (debug)
      println(s"assert yieldval: expected ${expected} yielded ${yielded}")
    assertEquals(Some(expected), yielded)
  }

  assert(!co.isDone())
  assertEquals(None, co.continue())
  assert(co.isDone())
}

  //takes a coroutine generator which returns coroutines always in the same state
def testOnRun[T](coroutineCreator: () => Coroutine[T]) = {
  val co1 = coroutineCreator()
  val co2 = coroutineCreator()

  val co1Results = mutable.Buffer[T]()
  val co2Results = mutable.Buffer[T]()

  def addToMutable(co: Coroutine[T], m: mutable.Buffer[T]): Option[T] = {
    val lastFetched = co.continue()
    lastFetched.foreach { value => m += value }
    lastFetched
  }

  while (addToMutable(co2, co2Results).isDefined) {}

  co1.run { value => co1Results += value }

  assertEquals(co1Results.length, co2Results.length)

  co1Results.lazyZip(co2Results) foreach { case (x1, x2) => 
    assertEquals(s"""$x1 is not equal to $x2""", x1, x2) 
  }

  Seq(co1, co2).foreach { co =>
    assert(co.isDone())
    assert(co.continue().isEmpty)
  }

}