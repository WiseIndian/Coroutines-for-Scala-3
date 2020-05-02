package coroutines

def yieldval[T](t: T): T = t

final case class YieldvalAtWrongLocationException(private val message: String = "", 
  private val cause: Throwable = None.orNull) extends Exception(message, cause) 

  
abstract class Coroutine[T] {
  protected lazy val body: Option[T]
  //TODO find a way to make next protected and still make it accessible within the transformBody method of Macros
  var next: () => Option[T] = () => body

  var _isDone = false
  
  def run(f: T => Unit): Unit = {
    var res = this.continue()
    while (res.nonEmpty) {
      f(res.get)
      res = this.continue()
    }
  }

  def continue(): Option[T] = this.next()

  def isDone(): Boolean = _isDone

}
 



