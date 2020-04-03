import org.junit.Test
import org.junit.Assert._
import coroutines._
import coroutines.Macros._

class Test1 {
  @Test def test1(): Unit = {
    val co = coroutine[Int]({
      print("hello world")
      yieldval(1+1)
      yieldval(2*2)
    })

    List(Some(2), Some(4), None).foreach { v => 
      assertEquals(co.continue, v)
    }
  }

   @Test def test2(): Unit = {
    val co = coroutine[Int]({
      print("hello world")
    })

    List(None).foreach { v => 
      assertEquals(co.continue, v)
    }
  }

  @Test def noBodyTest(): Unit = {
    val co = coroutine[Int]({
    })

    List(None).foreach { v => 
      assertEquals(co.continue, v)
    }
  }


  @Test def withValsTest1(): Unit = {
    val co = coroutine[Int]({
      val x = 3
      yieldval(x)
    })

    List(Some(3), None).foreach { v => 
      assertEquals(co.continue, v)
    }
  }

  @Test def withValsTest2(): Unit = {
    val co = coroutine[Int]({
      val x = 3
      yieldval(x+1)
      // yieldval(x+1)
    })

    List(Some(4), None).foreach { v => 
      assertEquals(co.continue, v)
    }
  }

  @Test def withValsTest3(): Unit = {
    val co = coroutine[Int]({
      val x = 3
      yieldval(x+1)
      yieldval(x+2)
    })
    //for this test to pass I have to take value definitions out of the if scope
    List(Some(4), Some(5), None).foreach { v => 
      assertEquals(co.continue, v)
    }
  }
}