import org.junit.Test
import org.junit.Assert._
import coroutines._
import coroutines.Macros._



class ASTCanonicalizationTest {


  @Test def testIfApp(): Unit = {
    val co = coroutine[Int] {
      yieldval{
        if (0 < { math.abs(-1); math.max(1, 2) }) 2 else 1
      }
    }

    assertYieldvals(2 :: Nil, co) 
  }

  @Test def testIfAppYield(): Unit = {
    val co = coroutine[Int] {
      val x = if (0 < { math.abs(-1); math.max(1, 2) }) 2 else 1
      yieldval(x)
      yieldval(-x)
    }

    assertYieldvals(2 :: -2 :: Nil, co)
 
  }

  @Test def testIfSelections(): Unit = {
    val co = coroutine[Int] {
      yieldval{
        if (0 < { math.abs(math.Pi) }) 2 else 1
      }
    } 
    assertYieldvals(2 :: Nil, co)
  }

  @Test def testIfSelectionsYield(): Unit = {
    val co = coroutine[Int] {
      val x = if (0 < { math.abs(math.Pi) }) 2 else 1
      yieldval(x)
      yieldval(-x)
    } 

    assertYieldvals(2 :: -2 :: Nil, co)
  }

  @Test def testIfUpdates(): Unit = {
    val co = coroutine[Int] {
      val xs = new Array[Int](2)
      yieldval{
        if (0 < { xs(0) = 1; xs(0) }) 2 else 1
      }
    }

    assertYieldvals(2 :: Nil, co) 
  }

  @Test def testIfBlockInTuple(): Unit = {
    val co = coroutine[Int] {
      yieldval{
        if (0 < ({ math.abs(1); math.abs(3) + 2 }, 2)._1) 2 else 1
      }
    }

    assertYieldvals(2 :: Nil, co) 
  }

  @Test def testIfWithAnotherIfStatementInCond(): Unit = {
    val co = coroutine[Int] {
      yieldval{
        if (0 < (if (math.abs(-1) > 5) 1 else 2)) 2 else 1
      }
    }
    
    assertYieldvals(2 :: Nil, co)
  }

  @Test def valueDeclIsLastStatement(): Unit = {
    val co = coroutine[Int] {
     val t = (2, 3)
     val (y, z) = t
    }


    assertYieldvals(Nil, co) 
  }

  @Test def coroutineCallableOutsideValueDeclaration(): Unit = {
    var y: Int = 0
    def setY(x: Int) = coroutine[Int] { y = x }
    val setTo5 = coroutine[Int] {
      setY(5).continue()
    }

    assertEquals(setTo5.continue(), None)
    assertEquals(y, 5)
  }

  @Test def coroutineCallableOutsideValueDeclarationAndYield(): Unit = {
    var y: Int = 0
    def setY(x: Int) = coroutine[String] { y = x }
    val c = coroutine[String] { 
      setY(5).continue()
      yieldval("OK")
      setY(-5).continue()
    }

    assertEquals(y, 0)
    assertEquals(c.continue(), Some("OK"))
    assertEquals(y, 5)
    assert(!c.isDone())
    assertEquals(c.continue(), None)
    assertEquals(y, -5)
  }

  @Test def coroutineShouldYieldInWhileLoop(): Unit = {
    def rube(x: Int) = coroutine[Int] {
      var i = 0
      while (i < x && x < math.abs(-15)) {
        yieldval(i)
        i += 1
      }
    }
    
    val c1 = rube(10)
    for (i <- 0 until 10) {
      assertEquals(c1.continue(), Some(i))
    }

    assertEquals(c1.continue(), None)
    assert(c1.isDone())


    val c2 = rube(20)
    assert(!c2.isDone())
    assertEquals(c2.continue(), None)
    assert(c2.isDone())
  }

  @Test def shouldYieldEverySecondElementOrZero(): Unit = {
    def rube(x: Int) = coroutine[Int] { 
      var i = 0
      while (i < x && x < math.abs(-15)) {
        if (i % 2 == 0) yieldval(i)
        i += 1
      }
    }

    val c1 = rube(10)
    for (i <- 0 until 10; if i % 2 == 0) {
      assert(!c1.isDone())
      assertEquals(c1.continue(), Some(i))
    }
    assert(!c1.isDone())
    assertEquals(c1.continue(), None)
    assert(c1.isDone())


    val c2 = rube(20)
    assert(!c2.isDone())
    assertEquals(c2.continue(), None)
    assert(c2.isDone())

  }

  @Test def yields1OrYield10Elems: Unit = {
    def rube(x: Int) = coroutine[Int] {
      var i = 1
      if (x > math.abs(0)) {
        while (i < x) {
          yieldval(i)
          i += 1
        }
      } else {
        yieldval(i)
      }
    }

    val c1 = rube(10)
    for (i <- 1 until 10) {
      assert(!c1.isDone())
      assertEquals(c1.continue(), Some(i))
    }
    assert(!c1.isDone())
    assertEquals(c1.continue(), None)
    assert(c1.isDone())

    val c2 = rube(-10)
    assertEquals(c2.continue(), Some(1))
    assertEquals(c2.continue(), None)
    assert(c2.isDone())
  }

  @Test def yieldsAbsoluteAndOriginalVal(): Unit = {
    def rube(x: Int) = coroutine { 
      yieldval(math.abs(x))
      x
    }

    val c = rube(-5)

    assertYieldvals(5 :: Nil, c)
  }

  @Test def shortCircuitingWorkingForAnd(): Unit = {
    var state = "untouched"
    def rube(x: Int) = coroutine[Int] {
      val negative = 
        if (x < 0 && { state = "touched"; true }) x
        else -x

      yieldval(negative)
    }
    
    val c0 = rube(5)
    assertYieldvals(-5::Nil, c0)
    assertEquals(state, "untouched")

    val c1 = rube(-5)
    assertYieldvals(-5::Nil, c1)
    assert(state == "touched")
  }

  @Test def shortCircuitingWorkingForOr(): Unit = {
    var state = "untouched"
    def rube(x: Int) = coroutine[Int] {
      yieldval {
        if (x > 0 || { state = "touched"; false }) x
        else -x
      }
    }
    
    val c0 = rube(5)
    assertYieldvals(5::Nil, c0)
    assert(state == "untouched")

    val c1 = rube(-5)
    assertYieldvals(5::Nil, c1)
    assert(state == "touched")
  }

  // @Test def doWhileBecomesWhileLoop(): Unit = {
  //   def rube(x: Int) = coroutine[Int] { 
  //     var i: Int = 0;
  //     do {
  //       yieldval(i)
  //       i += 1
  //     } while (i < x);
  //   }

  //   val c0 = rube(5)
  //   assertYieldvals(0::1::2::3::4::Nil, c0) 

  //   val c1 = rube(0)
  //   assertYieldvals(0::Nil, c1) 
  // }

  @Test def canDefineUncalledFunctions(): Unit = {
    val c = coroutine[Int] { 
      def foo(): String = "bar"
      val bar = "bar"

      yieldval(1)
    }
    
    assertYieldvals(1::Nil, c)
  }

  @Test def canDefineCalledFunctions(): Unit = {
    val c = coroutine[Int] { 
      def foo(): String = "bar"
      val bar = foo()

      yieldval(1)
    }

    assertYieldvals(1::Nil, c)
  }
}
