import org.junit.Test
import org.junit.Assert._
import coroutines._
import coroutines.Macros._

class RegressionTest {
    @Test def testIfElse: Unit = {
    def xOrY(x: Int, y: Int) = coroutine[Int] { 
      if (x > 0) {
        yieldval(x)
      } else {
        yieldval(y)
      }
    }

    val c1 = xOrY(5, 2)
    assertYieldvals(5 :: Nil, c1)


    val c2 = xOrY(-2, 7)
    assertYieldvals(7::Nil, c2)
  }


  @Test def testIfsInWhile: Unit = {
    val numbers = coroutine[Int] { 
      var z = 1
      var i = 1
      while (i < 5) {
        if (z > 0) {
          yieldval(z * i)
          z = -1
        } else {
          yieldval(z * i)
          z = 1
          i += 1
        }
      }
    }

    val expected = (1 until 5).flatMap (i => List(i, -i)).toList
    assertYieldvals(expected, numbers)
  }

  @Test def testCoroutineCallingOtherOneWithDifferentType: Unit = {
    def powerer(x: Int) = coroutine[Int] { yieldval(x * x) }
    def caller(x: Int) = coroutine[Int] { 
      powerer(x).continue() match {
        case Some(powered) => yieldval(powered)
        case None => throw new Exception("Unexpected")

      }
    }

    assertYieldvals(5 * 5 :: Nil, caller(5))
  }

  
  @Test def testIssue14SimpleCase(): Unit = {
    object Test {
      def foo(i: Int): Coroutine[Int] = coroutine[Int] { 
        yieldval(i)
        if (i > 0) {
          join(foo(i - 1))
          join(foo(i - 1))
        }
      }
    }

    val c = Test.foo(2)

    assertYieldvals(2 :: 1 :: 0 :: 0 :: 1 :: 0 :: 0 :: Nil, c) 
  }

  @Test def testIssue14ComplexCase(): Unit = {
    object Test {
      def foo(i: Int): Coroutine[Int] = coroutine[Int] {
        yieldval(i)
        if (i > 0) {
          join(foo(i - 1))
          join(foo(i - 1))
        }
      }
    }

    val bar = coroutine[Int] { 
      join(Test.foo(2))
      join(Test.foo(2))
    }

    assertYieldvals(2::1::0::0::1::0::0::2::1::0::0::1::0::0::Nil, bar)
  }

  @Test def testIssue15Hygiene(): Unit = {
    val scala, Any, String, TypeTag, Unit = ()
    trait scala; trait Any; trait String; trait TypeTag; trait Unit

    def id(x: Int) = coroutine[Int] { 
      x
    }
  }

  @Test def testIssue15MoreHygiene(): Unit = {
    val org, coroutines, Coroutine = ()
    trait org; trait coroutines; trait Coroutine
    
    val id = coroutine[Int] { }
  }

  
  // @Test def testShouldUseCAsAnArgumentName(): Unit = {
  //   def nuthin = coroutine[Unit] {  }
  //   def resumer = coroutine { (c: Nothing <~> Unit) =>
  //     c.resume
  //   }
  //   val c = nuthin()
  //   val r = call(resumer(c))
  //   assert(!r.resume)
  //   assert(!r.hasException)
  //   assert(r.hasResult)
  // }

  @Test def testIssue21 = {
    def test = coroutine[Int] { }
    def foo: Coroutine[Int] = coroutine[Int] { 
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test)
      join(test) // Lines after this did not previously compile.
      join(test)
      join(test)
    }
  }

  //this doesnt work yet
  @Test def testMustCatchExceptionPassedFromADirectCall = {
    def buggy = coroutine[String] { 
      throw new Exception
    }

    def catchy = coroutine[String] {
      var result = "initial value"
      try {
        join(buggy)
        "not ok..."
      } catch {
        case e: Exception =>
          result = "caught!"
      }
      yieldval(result)
    }

    assertEquals(Some("caught!"), catchy.continue())
  }

}