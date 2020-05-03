import org.junit.Test
import org.junit.Assert._
import coroutines._
import coroutines.Macros._

class PatternMatchTest {
  def rube1(x: AnyRef) = coroutine[Int] {
    val res = x match {
      case s: String   => s.length
      case xs: List[_] => xs.size
    }
    yieldval(res)
  }

  @Test def test1: Unit = {
    val c = rube1("ok")
    assert(!c.isDone())
    assertEquals(c.continue(), Some(2))
    assert(!c.isDone())
    assertEquals(c.continue(), None)
    assert(c.isDone())
  }

  @Test def test2: Unit = {
    val c = rube1(1 :: 2 :: 3 :: Nil)
    assert(!c.isDone())
    assertEquals(c.continue(), Some(3))
    assert(!c.isDone())
    assertEquals(c.continue(), None)
    assert(c.isDone())
  }

  def rube2(x: AnyRef) = coroutine[Int] {
    x match {
      case s: String   => yieldval(s.length)
      case xs: List[_] => yieldval(xs.size)
    }
    yieldval(17)
  }
  @Test def test3: Unit = {
    val c = rube2("ok")
    assert(!c.isDone())
    assertEquals(c.continue(), Some(2))
    assert(!c.isDone())
    assertEquals(c.continue(), Some(17))
    assert(!c.isDone())
    assertEquals(c.continue(), None)
    assert(c.isDone())
  }

  @Test def test4: Unit = {
    val c = rube2(1 :: 2 :: 3 :: Nil)
    assert(!c.isDone())
    assertEquals(c.continue(), Some(3))
    assert(!c.isDone())
    assertEquals(c.continue(), Some(17))
    assert(!c.isDone())
    assertEquals(c.continue(), None)
    assert(c.isDone())
  }
}
