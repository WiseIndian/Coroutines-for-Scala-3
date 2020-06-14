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


}