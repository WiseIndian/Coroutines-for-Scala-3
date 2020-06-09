package coroutines

/**
  * The `yieldval` function is used to suspend the execution of the coroutine instance in which it is located. The yieldval
  * function is also used in order to return the value passed to `yieldval` as argument.
  * @param t the value to yield from the coroutine body.
  * @example {{{
  *   coroutine[Int] { 
  *     yieldval(1)
  *     println("hello") 
  *     yieldval(2) 
  *   }
  *   //This coroutine if executed (e.g. by calling continue on it) will return the value 1 and suspend itself, until it is resumed with continue again.
  * }}} 
  * 
  */
def yieldval[T](t: T): T = t

/**
 * The `join` function starts the @param subCoroutine execution. If @param subCoroutine yields any values and suspends itself,
 * then the coroutine in which `join` appears will suspend itself and return the value returned by `subCoroutine`.
 * 
 * Note that subCoroutine has to return values that are subtypes of the coroutine in which the join appears.
 * 
 * @example {{{
 *  val child = coroutine[Int] {
 *    yieldval(1)
 *    yieldval(2)
 *  }
 * 
 *  val parent = coroutine[Int] {
 *    yieldval(0)
 *    join(child)
 *    yieldval(3)
 *    
 *  }
 *  //If you consume all elements of parents you would see: 0, 1, 2, 3.
 * }}}
 * 
 * 
 * 
 * This method is semantically equivalent to 
 * {{{
 *   do {
 *    var subYield: Option[T] = subCoroutine.continue()
 *    if (subYield.isDefined) yieldval(subYield.get) 
 *   } while(subYield.isDefined)
 * }}}
 */
def join[T](subCoroutine: Coroutine[T]): Unit = {}

final case class YieldvalAtWrongLocationException(private val message: String = "", 
  private val cause: Throwable = None.orNull) extends Exception(message, cause) 

  
/**
  * This class contains the state of a coroutine. To actually create a coroutine use the 
  * coroutine helper method.
  * @example {{{
  *   val co: Coroutine[Int] = coroutine[Int]{ 
  *     yieldval(0) 
  *   }
  * }}}
  */
abstract class Coroutine[T] {
  protected lazy val body: Option[T]
  //TODO find a way to make next protected and still make it accessible within the transformBody method of Macros
  var next: () => Option[T] = () => body

  var _isDone = false
  
  /**
  * This method applies the function @param f on all element of this coroutine.
  * Note that this consumes all elements of the coroutine
  */
  def run(f: T => Unit): Unit = {
    var res = this.continue()
    while (res.nonEmpty) {
      f(res.get)
      res = this.continue()
    }
  }

  /**
   * @return the next element this coroutine yields wrapped in Some. If the coroutine reached the end of its execution this 
   * method will return None forever.
   */
  def continue(): Option[T] = this.next()

  /**
  * @return true if all elements of this coroutine have been consumed. If the `continue()` method has not returned None yet this is false.  
  * Once the coroutine returns None in the continue method for the first time
  */
  def isDone(): Boolean = _isDone

}
 



