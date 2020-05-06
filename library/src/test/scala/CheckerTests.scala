import org.junit.Test
import org.junit.Assert._
import coroutines._
import coroutines.Macros._

class CheckerTests {
//TODO assert all those fail!
  @Test def methodDef(): Unit = {
    coroutine[Int] {
      def foo() {
        yieldval(1)
      }
    }
  }

  @Test def rhsAssignment(): Unit = {
    coroutine[Int] {
      val x = {
        yieldval(3)
        1
      }
    }
  }

  class A {
    var x: Int = 0

    def foo(x: Int, y: Int): Int = 0
  }
  @Test def objAssignment1(): Unit = {
    coroutine[Int] {
      new A().x = {
        yieldval(1)
        1
      }
    }
  }

  @Test def objAssignment2(): Unit = {
    coroutine[Int] {
      { yieldval(1); new A() }.x = 1
    }
  }

  @Test def methodCall1(): Unit = {
    coroutine[Int] {
      val a = new A()

      { yieldval(1); a }.foo(0, 0)
    }
  }

  @Test def methodCall2(): Unit = {
    coroutine[Int] {
      val a = new A()

      a.foo({ yieldval(1); 0 }, 2)
    }
  }
  @Test def methodCall3(): Unit = {
    coroutine[Int] {
      val a = new A()

      a.foo(0, { yieldval(1); 2 })
    }
  }

  @Test def ifCond(): Unit = {
    coroutine[Int] {
      if ({ yieldval(0); true }) 1
    }
  }

  @Test def whileCond(): Unit = {
    coroutine[Int] {
      while ({ yieldval(0); true }) 1
    }
  }

  class B(val x: Int)

  @Test def classCreation(): Unit = {
    coroutine[Int] {
      new B({ yieldval(0); 1 })
    }
  }

  @Test def classAttribute(): Unit = {
    coroutine[Int] {
      val b = new B(0) { yieldval(0); b }.x
    }
  }

  @Test def anonymousFun(): Unit = {
    coroutine[Int] { () => { yieldval(0); 1 } }
  }

  @Test def anonymousFun2(): Unit = {
    coroutine[Int] { () => { yieldval(0) } }
  }

  @Test def patternMatching1(): Unit = {
    coroutine[Int] {
      { yieldval(0); "oss117" } match {
        case "oss117" => 1
        case _        => 2
      }

    }
  }

  @Test def patternMatching2(): Unit = {
    coroutine[Int] {
      "oss117" match {
        case "oss117" if { yieldval(0); 1 == 1 } => 1
        case _                                   => 2
      }

    }
  }

  @Test def tryCatch1(): Unit = {
    coroutine[Int] {
      try {
        1 + 1
      } catch {
        case e: Exception => yieldval(0)
      }

    }
  }

  @Test def tryCatch2(): Unit = {
    coroutine[Int] {
      try {
        yieldval(0)
        1 + 1
      } catch {
        case e: Exception => 1
      }
    }
  }

  @Test def ifIf(): Unit = {
    coroutine[Int] {
        if ( 
            if ({yieldval(0); true}) true else false          
        ) 1 else 2
    }
  }
}
