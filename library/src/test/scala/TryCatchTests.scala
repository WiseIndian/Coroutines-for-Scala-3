import org.junit.Test
import org.junit.Assert._
import coroutines._
import coroutines.Macros._

class TryCatchTests {
    @Test def tryNoYield: Unit = {
        var x: Int = 0
        var y: Int = 0
        val co = coroutine[Int] { 
            yieldval(0)
            try {
                y = 1
            } catch { case e: Throwable => 
                x = 1
            }
            yieldval(1)
        }

        assertYieldvals(0::1::Nil, co)
        assertEquals(x, 0)
        assertEquals(y, 1)
 
    }

    @Test def tryYield: Unit = {
        var x: Int = 0
        var y: Int = 0
        val co = coroutine[Int] { 
            yieldval(0)
            try {
                yieldval(1)
                yieldval(2)
                y = 1
            } catch { case e: Throwable => 
                x = 1
            }
            yieldval(3)
        }

        assertYieldvals(0::1::2::3::Nil, co)
        assertEquals(x, 0)
        assertEquals(y, 1)
    }

    @Test def tryInTheWild: Unit = {
        var x: Int = 0
        var y: Int = 0
        var i: Int = 0
        val co = coroutine[Int] { 
            yieldval(-1)
            while(i < 10) {
                yieldval(i)
                try {
                    println("pwet")
                    yieldval(i+1)
                    yieldval(i+2)
                    y += 1
                } catch { case e: Throwable => 
                    x += 2
                }
                yieldval(i+3)

                i += 1
            }
            yieldval(-2)
        }

        val expectedOutput = (0 until 10).flatMap{ i => 
            i :: (i+1) :: (i+2) :: (i+3) :: Nil
        }

        assertYieldvals(List(-1) ++ expectedOutput.toList ++ List(-2), co, debug=true)
        assertEquals(0, x)
        assertEquals(10, y)
    }

    @Test def tryInWhileAndIf: Unit = {
        var x: Int = 0
        var y: Int = 0
        var i: Int = 0
        val co = coroutine[Int] { 
            if (x == 0 ) {
                yieldval(-1)
                while(i < 10) {
                    yieldval(i)
                    try {
                        println("pwet")
                        yieldval(i+1)
                        yieldval(i+2)
                        y += 1
                    } catch { case e: Throwable => 
                        x += 2
                    }
                    yieldval(i+3)

                    i += 1
                }
                yieldval(-2)
            } else {
                yieldval(666)
            }
        }

        val expectedOutput = (0 until 10).flatMap{ i => 
            i :: (i+1) :: (i+2) :: (i+3) :: Nil
        }

        assertYieldvals(List(-1) ++ expectedOutput.toList ++ List(-2), co, debug=true)
        assertEquals(0, x)
        assertEquals(10, y)
    }

    @Test def assertCatchCatches(): Unit = {
        var caught = false
        var finalized = false
        val co = coroutine[Int] {
            try {
                yieldval(0)
                throw new RuntimeException("Oups")
                yieldval(1)
            } catch {
                case e: Throwable => caught = true
            } finally {
                finalized = true
            }
        }
        assertEquals(Some(0), co.continue())
        assertEquals(None, co.continue())
        assertEquals(true, caught)
        assertEquals(true, finalized)
    }



}