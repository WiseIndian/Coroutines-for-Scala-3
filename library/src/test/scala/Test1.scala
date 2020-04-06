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



  @Test def testIfDesign1(): Unit = {
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
    val co = new IfCoroutineExample1(new B())
    assertEquals(co.continue, Some(10))
    assertEquals(co.continue, Some(new B().y))
    assertEquals(co.continue, None)
  }

  @Test def testIfDesign2(): Unit = { 
    val b = new B()
    val co = new IfCoroutineExample1(b)
    assertEquals(co.continue, Some(10))
    b.y = 111
    assertEquals(co.continue, Some(new B().y))
    assertEquals(co.continue, None)
  }

  @Test def testIfDesign3(): Unit = { 
    val b = new B()
    b.y = 111
    val co = new IfCoroutineExample1(b)
    assertEquals(co.continue, Some(111))
    assertEquals(co.continue, None)
  }
 

  @Test def testIfDesign4(): Unit = {
    /*
    coroutine[Int] {
      if (x < 10) {
          yieldval(1)
      } else {
          yieldval(2)
      }
    }
    */
    val co = new IfCoroutineExample2(x = 0)
    List(Some(1), None) foreach(assertEquals(_, co.continue))
  }




  @Test def testIfDesign5(): Unit = {
  /*
coroutine[Int] {
    
    if (x < 10) {
        if (y < 10) {
            yieldval(1)
            yieldval(2)
        } else {
            yieldval(3)
        }
    } else {
        yieldval(4)
    }
} 
*/
    val co = new IfCoroutineExample3(x = 0, y = 0)
    List(Some(1), Some(2), None, None, None) foreach(assertEquals(_, co.continue))
  }


  @Test def testIfDesign6(): Unit = {
    val co = new IfCoroutineExample3(x = 0, y = 10)
    List(Some(3), None, None) foreach(assertEquals(_, co.continue))
  }


  @Test def testIfDesign7(): Unit = {
    val co = new IfCoroutineExample3(x = 10, y = 10)
    List(Some(4), None, None) foreach(assertEquals(_, co.continue))
  }


  @Test def testIfDesignScope(): Unit = {
  // coroutine[Int] {
  //   val x = 0
  //   if (x == 0) {
  //     val x = 1
  //     yieldval(x)
  //   }

  //   yieldval(x)
  // }


    val co = IfCoroutineExample4()
    List(Some(1), Some(0), None)  foreach(assertEquals(_, co.continue))
  }

//TODO recreate if design tests for the case when we'll have automated the creation of coroutine with if in their body.

  // @Test def withValsTest3(): Unit = {
  //   val co = coroutine[Int]({
  //     val x = 3
  //     yieldval(x+1)
  //     yieldval(x+2)
  //   })
  //   //for this test to pass I have to take value definitions out of the if scope
  //   List(Some(4), Some(5), None).foreach { v => 
  //     assertEquals(co.continue, v)
  //   }
  // }

  // @Test def testIf1(): Unit = {
  //   val b: B = new B()


  //   val co = coroutine[Int] ({
  //     val bVal = b.y 
  //     if (b.y < 10) {
  //       val x = 3
  //       yieldval(10)
  //       println(x)
  //     }
  //     yieldval(bVal)
  //   })

  //   assertEquals(co.continue, 10)
  //   b.y = 11
  //   assertEquals(co.continue, new B().y)
  // }

  // @Test def testIf2(): Unit = {
  //   val b: B = new B()


  //   val co = coroutine[Int] ({
  //     val bVal = b.y 
  //     if (b.y < 10) {
  //       val x = 3
  //       yieldval(10)
  //       println(x)
  //     }
  //     yieldval(bVal)
  //   })

  //   assertEquals(co.continue, 10)
  //   assertEquals(co.continue, new B().y)
  // }


}