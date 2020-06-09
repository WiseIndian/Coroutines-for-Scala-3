import org.junit.Test
import org.junit.Assert._
import coroutines._
import coroutines.Macros._

class StackfulTests {

  @Test def simpleTest: Unit = {
    val sub = coroutine[Int] {
      yieldval(1)
    }

    val co = coroutine[Int] {
      yieldval(0)
      join(sub)
      yieldval(2)

    }

    assertYieldvals(List(0, 1, 2), co)

  }

  @Test def simpleTestOnRunMethod: Unit = {
    def getCo() = {
      val sub = coroutine[Int] {
        yieldval(1)
      }
  
      coroutine[Int] {
        yieldval(0)
        join(sub)
        yieldval(2)
  
      }
    }

    testOnRun(getCo)
  }

  class A(val x: Int) {
    override def toString(): String = s"""A($x)"""

    override def equals(o: Any): Boolean = {
      o.isInstanceOf[A] && o.asInstanceOf[A].x == x
    }
  }
  class B(x: Int) extends A(x) {
    override def toString(): String = s"""B($x)"""

    override def equals(o: Any): Boolean = {
      o.isInstanceOf[B] && o.asInstanceOf[B].x == x
    }
  }
  class C(x: Int) extends B(x) {
    override def toString(): String = s"""C($x)"""

    override def equals(o: Any): Boolean = {
      o.isInstanceOf[C] && o.asInstanceOf[C].x == x
    }
  }

  @Test def subTypingNesting: Unit = {
    val sub2 = coroutine[C] {
      yieldval(new C(1))
      yieldval(new C(2))
    }

    val sub3 = coroutine[C] {
      yieldval(new C(4))
    }

    val sub1 = coroutine[B] {
      join(sub2)
      yieldval(new B(3))
      join(sub3)
      yieldval(new C(5))

    }

    val co = coroutine[A] {
      yieldval(new A(0))
      join(sub1)
      yieldval(new C(6))
    }

    assertStackful(0 to 6, co)
  }

  @Test def testSubtypeNestingOnRun: Unit = {
    def getCo() = {
      val sub2 = coroutine[C] {
        yieldval(new C(1))
        yieldval(new C(2))
      }
  
      val sub3 = coroutine[C] {
        yieldval(new C(4))
      }
  
      val sub1 = coroutine[B] {
        join(sub2)
        yieldval(new B(3))
        join(sub3)
        yieldval(new C(5))
  
      }
  
      coroutine[A] {
        yieldval(new A(0))
        join(sub1)
        yieldval(new C(6))
      }
    }

    testOnRun(getCo)
  }

  def assertStackful(expected: Seq[Int], co: Coroutine[A]): Unit = {
    expected.foreach { i =>
      assert(!co.isDone())
      val opt = co.continue()
      assert(opt.isDefined)
      opt.foreach { a => assertEquals(i, a.x) }
    }
    assertEquals(None, co.continue())
    assert(co.isDone())
  }

  @Test def withIf(): Unit = {
      def sub(b: Boolean) = coroutine[B] {
          yieldval(new B(1))
          if (b) {
            yieldval(new B(20))
          } else {
            yieldval(new B(10))
          }
          yieldval(new B(30))
      }

      def co(b: Boolean) = coroutine[A] {
          yieldval(new A(0))
          join(sub(b)) 
          print("hey")
          yieldval(new B(40))
      }
 
      assertStackful(List(0, 1, 10, 30, 40), co(false))
      assertStackful(List(0, 1, 20, 30, 40), co(true))
  }

  @Test def withWhile(): Unit = {
      def sub(limit: Int) = coroutine[Int] {
        yieldval(-2)

        var i = 0
        while (i < limit) {
            yieldval(2 * i)
            i += 1
        }

        yieldval(-3)
      }

      def co(limit: Int) = coroutine[Int] {
        yieldval(-1)
        join(sub(limit))
        yieldval(-4)
      }

      assertYieldvals(List(-1, -2) ++ (0 until 10).map(_*2) ++ List(-3,-4), co(10))
  }
  //test mixing other features of the language with stackfulness

}
