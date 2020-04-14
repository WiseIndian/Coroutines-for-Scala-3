package coroutines

def yieldval[T](t: T): T = t

abstract class Coroutine[T] {
  protected val body: Option[T]
  //TODO find a way to make next protected and still make it accessible within the transformBody method of Macros
  var next: () => Option[T] = () => body
  
  def run(f: T => Unit): Unit = {
    var res = this.continue()
    while (res.nonEmpty) {
      f(res.get)
      res = this.continue()
    }
  }

  def continue(): Option[T] = this.next()

  def isDone(): Boolean = this.next == null

}
 



