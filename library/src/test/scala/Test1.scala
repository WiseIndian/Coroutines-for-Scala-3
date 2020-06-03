import org.junit.Test
import org.junit.Assert._
import coroutines._
import coroutines.Macros._

class Test1 {

  @Test def ifElse(): Unit = {
    def co(b: Boolean) = coroutine[Int] {
      yieldval(1)
      if (b) {
        yieldval(2)
      } else {
        yieldval(3)
      }
      yieldval(4)
    }

    assertYieldvals(List(1,2,4), co(true))
    assertYieldvals(List(1,3,4), co(false))
  }

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
    assert(co.isDone())
  }


  def getWhileCoroutine1(initialX: Int) = {
    coroutine[Int] {
      var x = initialX
      while (x < 10) {
        yieldval(x)
        x += 1
      }
    }
  }

 
  @Test def testWhile1(): Unit = {
    val co = getWhileCoroutine1(11)
    (0 until 3).foreach(_ => assertEquals(None, co.continue()))
  }

  @Test def testWhile2(): Unit = {
    val co = getWhileCoroutine1(initialX = 0) 
    (0 until 10).foreach { i => 
      assertEquals(Some(i), co.continue())
    }
    assertEquals(None, co.continue())
    assert(co.isDone())
  }

  @Test def testWhilePair(): Unit = {
    val co = coroutine[Int] {
      var x = 0
      while (x < 10) {
        if (x % 2 == 0) {
          yieldval(x)
        }
        x += 1
      }
    }

    (0 until 5).foreach(i => assertEquals(Some(2*i), co.continue()))
    assertEquals(None, co.continue())
    assert(co.isDone())
  }
 
  //TODO hum how can i can an exception occuring during compilation..
  // @Test(expected = classOf[YieldvalAtWrongLocationException])
  // def shouldThrowIfYieldvalWithinWhileCond(): Unit = {
  //     val co = coroutine[Int] {
  //       while (yieldval(1) == 2){
  //         //whatever
  //       }
  //     }
  // }

  //this will only work if we transform the meaning of yieldval within functions and within function calls.
  // @Test def traverseList(): Unit =  {
  //   def getIterator(ls: List[String]) = coroutine[String] {
  //     def foreach(f: String => Unit): Unit = {
  //       var cur = ls
  //       while (!cur.isEmpty) {
  //         f(cur.head)
  //         cur = cur.tail
  //       }
  //     }

  //     foreach(e => yieldval(e))
  //   }

  //   val ls = List("one","two","three","four")
  //   val iterator = getIterator(ls)

  //   ls.foreach { s => 
  //     val opt = iterator.continue()
  //     assert(opt.isDefined)
  //     opt.map(yielded => assertEquals(s, yielded))
  //   }

  //   assertEquals(None, iterator.continue())
  //   assert(iterator.isDone())
  // }

  def getCaseCoroutine(ls: List[Any]): Coroutine[Int] = coroutine[Int] {
    ls match {
      case "Hello" :: Nil => yieldval(0)
      case "Hello" :: tail => 
        yieldval(1)
        tail match {
          case 1 :: true :: Nil =>  yieldval(2) 
          case 1 :: Nil =>  yieldval(3)
          case _ => yieldval(4)
        }
        yieldval(5)

      case _ => yieldval(6)
    }
  }
  

  @Test def caseTest0(): Unit =  {
    val co = getCaseCoroutine(List("Hello"))
    List(Some(0), None).foreach(assertEquals(_, co.continue()))
  }

  @Test def caseTest1(): Unit = {
    val co = getCaseCoroutine(List("Hello", 1, true))
    List(Some(1), Some(2), Some(5), None).foreach(assertEquals(_, co.continue()))
  }

  @Test def caseTest2(): Unit =  {
    val co = getCaseCoroutine(List("Hello", 1))
    List(Some(1), Some(3), Some(5), None).foreach(assertEquals(_, co.continue()))
  }


  @Test def caseTest3(): Unit =  {
    val co = getCaseCoroutine(List("Hello", "Whatever"))
    List(Some(1), Some(4), Some(5), None).foreach(assertEquals(_, co.continue()))
  }


  @Test def caseTest4(): Unit =  {
    val lists: List[List[Any]] = List() :: List("hello", "world") :: Nil
    lists.foreach { ls => 
      val co = getCaseCoroutine(ls)
      List(Some(6), None).foreach(assertEquals(_, co.continue()))
    }
  }

  // @Test def traverseCollectionTest(): Unit = {
    
  //   def getIterator[T](ls: List[T]) = coroutine[T] {
  //     def foreach(f: T => Unit): Unit = {
  //       var cur = ls
  //       while (!cur.isEmpty) {
  //         f(cur.head)
  //         cur = cur.tail
  //       }
  //     }

  //     foreach(element => yieldval(element))
  //   }

  //   val iterator1 = getIterator(List(1,2,3,4))
  //   val iterator2 = getIterator(List("one", "two", "three"))

  //   val expected = Seq("1 one", "2 two", "3 three")
  //   var output = Seq()
  //   while(!iterator1.isDone() && !iterator2.isDone()) {
  //     output :+ (iterator1.continue().get + " " + iterator2.continue().get)
  //   }
    
  //   expected.zip(output).foreach { case (e, o) => 
  //     assertEquals(e, o)
  //   }
  // }
}