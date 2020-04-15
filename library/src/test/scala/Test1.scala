import org.junit.Test
import org.junit.Assert._
import coroutines._
import coroutines.Macros._

class Test1 {
  @Test def test1(): Unit = {
    val co = coroutine[Int]({
      print("hello world")
      yieldval(2*2)
      yieldval(3)
    })

    List(Some(4), Some(3), None).foreach { v => 
      assertEquals(co.continue(), v)
    }
  }

   @Test def test2(): Unit = {
    val co = coroutine[Int]({
      print("hello world")
    })

    List(None).foreach { v => 
      assertEquals(co.continue(), v)
    }
  }

  @Test def noBodyTest(): Unit = {
    val co = coroutine[Int]({
    })

    List(None).foreach { v => 
      assertEquals(co.continue(), v)
    }
  }


  @Test def withValsTest1(): Unit = {
    val co = coroutine[Int]({
      val x = 3
      yieldval(x)
    })

    List(Some(3), None).foreach { v => 
      assertEquals(co.continue(), v)
    }
  }

  @Test def withValsTest2(): Unit = {
    val co = coroutine[Int]({
      val x = 3
      yieldval(x+1)
      // yieldval(x+1)
    })

    List(Some(4), None).foreach { v => 
      assertEquals(co.continue(), v)
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
      assertEquals(co.continue(), v)
    }
  }

  class B {
    var y = 0
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

    assertEquals(Some(10), co.continue())
    b.y = 11
    assertEquals(Some(new B().y), co.continue())
    assertEquals(None, co.continue())
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

    assertEquals(co.continue(), Some(10))
    assertEquals(co.continue(), Some(new B().y))
    assertEquals(co.continue(), None)
  }




  @Test def testIfDesign4(): Unit = {
    
    val co = coroutine[Int] {
      val x = 0
      if (x < 10) {
          yieldval(1)
      } else {
          yieldval(2)
      }
    }
    
    List(Some(1), None) foreach(assertEquals(_, co.continue()))
  }



  def getCoroutine1(x: Int, y: Int): Coroutine[Int] = 
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

  @Test def testIfDesign5(): Unit = {
  
    val co = getCoroutine1(0,0);

    List(Some(1), Some(2), None, None, None) foreach(assertEquals(_, co.continue()))
  }


  @Test def testIfDesign6(): Unit = {
    val co = getCoroutine1(x = 0, y = 10)
    List(Some(3), None, None) foreach(assertEquals(_, co.continue()))
  }


  @Test def testIfDesign7(): Unit = {
    val co = getCoroutine1(x = 10, y = 10)
    List(Some(4), None, None) foreach(assertEquals(_, co.continue()))
  }


  @Test def testIfDesignScope1(): Unit = {
    val co = coroutine[Int] {
      val x = 0
      if (x == 0) {
        val x = 1
        yieldval(x)
      }

      yieldval(x)
    }


    List(Some(1), Some(0), None)  foreach(assertEquals(_, co.continue()))
  }

  def getCoroutine2(initialX: Int): Coroutine[Int] = {
    coroutine[Int] {
        val x = initialX
        if (x == 0) {
            val x = 1
            yieldval(x)
            if (x == 1) {
                val x = 2
                yieldval(x)
            }
            yieldval(x)
        }
        yieldval(x)
    }
  }

  @Test def testIfDesignScope2(): Unit = {
    val co = getCoroutine2(initialX = 0)
    List(Some(1), Some(2), Some(1), Some(0), None).foreach(assertEquals(_, co.continue()))
  }

  @Test def testIfDesignScope3(): Unit = {
    val co = getCoroutine2(initialX = 5)
    List(Some(5), None).foreach(assertEquals(_, co.continue()))
  }













//TODO recreate if design tests for the case when we'll have automated the creation of coroutine with if in their body.

}