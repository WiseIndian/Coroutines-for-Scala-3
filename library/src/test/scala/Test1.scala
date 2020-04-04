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



  @Test def testIfDesign(): Unit = {
    /* `co` is supposed to be the translation of:
    coroutine[Int] {
      val bVal = b.y 
      if (b.y < 10) {
        val x = 3
        yieldval 10
        println(x)
      }
      yieldval bVal
    }
    */
    val co = new IfCoroutine(new B())
    assertEquals(co.continue, Some(10))
    assertEquals(co.continue, Some(new B().y))
    assertEquals(co.continue, None)
  }

  @Test def testIfDesign2(): Unit = { 
    val b = new B()
    val co = new IfCoroutine(b)
    assertEquals(co.continue, Some(10))
    b.y = 111
    assertEquals(co.continue, Some(new B().y))
    assertEquals(co.continue, None)
  }

  @Test def testIfDesign3(): Unit = { 
    val b = new B()
    b.y = 111
    val co = new IfCoroutine(b)
    assertEquals(co.continue, Some(111))
    assertEquals(co.continue, None)
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

  @Test def testIf1(): Unit = {
    val b: B = new B()


    val co = coroutine[Int] ({
      val bVal = b.y 
      if (b.y < 10) {
        val x = 3
        yieldval(10)
        println(x)
      }
      yieldval(bVal)
    })

    assertEquals(co.continue, 10)
    b.y = 11
    assertEquals(co.continue, new B().y)
  }

  @Test def testIf2(): Unit = {
    val b: B = new B()


    val co = coroutine[Int] ({
      val bVal = b.y 
      if (b.y < 10) {
        val x = 3
        yieldval(10)
        println(x)
      }
      yieldval(bVal)
    })

    assertEquals(co.continue, 10)
    assertEquals(co.continue, new B().y)
  }


}