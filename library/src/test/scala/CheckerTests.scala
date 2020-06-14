import org.junit.Test
import org.junit.Assert._
import coroutines._
import coroutines.Macros._
import scala.compiletime.testing._

class CheckerTests {
//TODO assert all those fail!
  @Test def methodDef(): Unit = {
    val typechecks: Boolean = typeChecks(
      """
      coroutine[Int] {
        def foo(): Unit = {
          yieldval(1)
        }
      }
      """
    )

    assert(!typechecks)
  }
  
  @Test def rhsAssignment(): Unit = {
    val error: Boolean = !typeChecks("""coroutineInt] {
      val x = {
        yieldval(3)
        1
      }
    }""")
    assert(error)
  }

  class A {
    var x: Int = 0

    def foo(x: Int, y: Int): Int = 0
  }
  @Test def objAssignment1(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      new A().x = {
        yieldval(1)
        1
      }
    }""")
    assert(error)
  }

  @Test def objAssignment2(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      { yieldval(1); new A() }.x = 1
    }""")
    assert(error)
  }

  @Test def methodCall1(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      val a = new A()

      { yieldval(1); a }.foo(0, 0)
    }""")
    assert(error)
  }

  @Test def methodCall2(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      val a = new A()

      a.foo({ yieldval(1); 0 }, 2)
    }""")

    assert(error)
  }
  @Test def methodCall3(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      val a = new A()

      a.foo(0, { yieldval(1); 2 })
    }""")

    assert(error)
  }

  @Test def ifCond(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      if ({ yieldval(0); true }) 1
    }""")

    assert(error)
  }

  @Test def whileCond(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      while ({ yieldval(0); true }) 1
    }""")

    assert(error)
  }

  class B(val x: Int)

  @Test def classCreation(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      new B({ yieldval(0); 1 })
    }""")

    assert(error)
  }

  @Test def classAttribute(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      
      { 
        val bVal = new B(0)
        yieldval(0)
        bVal 
      }.x
    }""")

    assert(error)
  }

  @Test def anonymousFun(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] { () => { yieldval(0); 1 } }""")
    assert(error)
  }

  @Test def anonymousFun2(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] { () => { yieldval(0) } }""")
    assert(error)
  }

  @Test def patternMatching1(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      { yieldval(0); "oss117" } match {
        case "oss117" => 1
        case _        => 2
      }

    }""")

    assert(error)
  }


  @Test def patternMatching2(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      "oss117" match {
        case "oss117" if true => 1
        case _                                   => 2
      }

    }""")

    assert(!error)
  }

  @Test def tryCatch1(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      try {
        1 + 1
      } catch {
        case e: Exception => yieldval(0)
      }
    }""")

    assert(error)
  }

  @Test def tryCatch2(): Unit = {
    val noError: Boolean = typeChecks("""coroutine[Int] {
      try {
        yieldval(0)
        1 + 1
      } catch {
        case e: Exception => 1
      }
    }""")

    assert(noError)
  }

  @Test def yieldvalInCatchCrashes(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      try {
        1 + 1
      } catch {
        case e: Exception => yieldval(2)
      }
    }""")

    assert(error)
  }

  @Test def yieldvalInGuardCrashes(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
      try {
        1 + 1
      } catch {
        case e: Exception if ({yieldval(2); true}) => 1
      }
    }""")

    assert(error)
  }

  @Test def yieldvalCatchCrashes(): Unit = {
    assert {
      !typeChecks("""coroutine[Int] {
        try {
          1 + 1
        } catch {
          case e: Exception => {
            1 + 1
            yieldval(1)
          }
        }
      }""")
    }
  }

  @Test def yieldvalInFinallyCrashes(): Unit = {
    assert {
      !typeChecks("""coroutine[Int] {
        try {
          1 + 1
        } catch {
          case e: Exception => 1
        } finally {
          yieldval(1)
        }
      }""")
    }
  }

  @Test def ifIf(): Unit = {
    val error: Boolean = !typeChecks("""coroutine[Int] {
        if ( 
            if ({yieldval(0); true}) true else false          
        ) 1 else 2
    }""")

    assert(error)
  }


  @Test def yieldvalTypeTestShouldBreak(): Unit = {
    assert { 
      !typeChecks{"""
        coroutine[Int] {
          yieldval("Will this break stuff? :o")
        }"""
      }
    
    }
  }

  class A1
  class A2 extends A1
  class A3 extends A2


  @Test def yieldvalTypeTestShouldBreak2(): Unit = {
    assert { 
      !typeChecks{"""
        coroutine[Int] {
          yieldval(1)
          print("hey")
          yieldval("This should break stuff")
        }"""
      }
    }
  }

  @Test def yieldvalTypeTestShouldBreak3(): Unit = {
    assert { 
      !typeChecks{"""
        coroutine[A1] {
          yieldval(new A2())
          print("hey")
          yieldval("This should break stuff")
        }"""
      }
    }
  }


  @Test def thisShouldTypeCheck(): Unit = {
    assert { 
      typeChecks{"""
        coroutine[A1] {
          yieldval(new A1())
          yieldval(new A2())
          print("hey")
          yieldval(new A3())
        }"""
      }
    }
  }



  @Test def yieldvalTypeTest3(): Unit = {
    assert { 
      !typeChecks{"""
        coroutine[A1] {
          yieldval(new A2())
          print("hey")
          yieldval("This should break stuff")
        }"""
      }
    }
  }



  @Test def yieldvalTypeTest4(): Unit = {
    assert { 
      !typeChecks{"""
        coroutine[A1] {
          yieldval("This should break stuff")
          yieldval(new A1())
          yieldval(new A2())
          print("hey")
        }"""
      }
    }
  }


  @Test def yieldvalArgTypeWithWhileTest(): Unit = {
    assert { 
      !typeChecks{"""
        coroutine[A1] {
          yieldval(new A3())
          while (true) {
            yieldval("This should break stuff")
          }
          yieldval(new A1())
          yieldval(new A2())
          print("hey")
        }"""
      }
    }
  }

  @Test def yieldvalArgTypeWithIfTest(): Unit = {
    assert { 
      !typeChecks{"""
        coroutine[A1] {
          yieldval(new A3())
          if (true) {
            yieldval("This should break stuff")
          }  
          yieldval(new A1())
          yieldval(new A2())
          print("hey")
        }"""
      }
    }
  }


  @Test def yieldvalArgTypeWithIfTest2(): Unit = {
    assert { 
      !typeChecks{"""
        coroutine[A1] {
          yieldval(new A3())
          if (true) {
            yieldval(new A1())
            yieldval("This should break stuff")
          }  
          yieldval(new A1())
          yieldval(new A2())
          print("hey")
        }"""
      }
    }
  }


  @Test def yieldvalArgTypeWithIfWhileTest2(): Unit = {
    assert { 
      !typeChecks{"""
        coroutine[A1] {
          yieldval(new A3())
          if (true) {
            yieldval(new A3())
            while(true) {
              yieldval("This should break stuff")
            }
            yieldval(new A1())
          }  
          yieldval(new A1())
          yieldval(new A2())
          print("hey")
        }"""
      }
    }
  }


  @Test def joinInt(): Unit = {
    assert { 
      typeChecks{"""
        val co1 = coroutine[Int] {}
        coroutine[Int]{join(co1)}
      """
      }
    }
  }


  // class A1
  // class A2 extends A1
  // class A3 extends A2


  @Test def joinSubclass(): Unit = {
    assert { 
      typeChecks{"""
        val co1 = coroutine[A2] {}
        coroutine[A1]{join(co1)}
      """
      }
    }
  }


  @Test def joinSubclass2(): Unit = {
    assert { 
      typeChecks{"""
        val co1 = coroutine[A3] {}
        coroutine[A1]{join(co1)}
      """
      }
    }
  }

    @Test def joinSuperclass(): Unit = {
      assert { 
        !typeChecks{"""
          val co1 = coroutine[A1] {}
          coroutine[A3]{join(co1)}
        """
        }
      }
    }

    @Test def joinSuperclass2(): Unit = {
      assert { 
        !typeChecks{"""
          val co1 = coroutine[A2] {}
          coroutine[A3]{join(co1)}
        """
        }
      }
    }

    @Test def joinSuperclassIf(): Unit = {
      assert { 
        !typeChecks{"""
          val co1 = coroutine[A2] {}
          coroutine[A3]{
            val b = true
            if (b) 
              join(co1)
          }
        """
        }
      }      
    }

    @Test def joinSuperclassNested(): Unit = {
      assert { 
        typeChecks{"""
          val co1 = coroutine[A3] {}
          val co2 = coroutine[A3] {join(co1)}

          coroutine[A2]{
            val b = true
            if (b) join(co2)
          }
        """
        }
      }      
    }

    @Test def joinSuperclassNested2(): Unit = {
      assert { 
        !typeChecks{"""
          val co1 = coroutine[A1] {}
          val co2 = coroutine[A3] {join(co1)}

          coroutine[A2]{
            val b = true
            if (b) join(co2)
          }
        """
        }
      }      
    }    

}
