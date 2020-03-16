import org.junit.Test
import org.junit.Assert._

class Test1 {
  @Test def t1(): Unit = {
    assertEquals("I was compiled by dotty :)", Main.msg)

    val co = coroutine[Int]({
      print("hello world")
      yieldval(1+1)
      yieldval(2*2)
    })

    List(Some(2), Some(4), None).foreach { v => 
      assertEquals(co.continue(), v)
    }
  }
}