class Coroutine[A, B, R](val f: A => R) {
  def call(input: A): Coroutine[A, B, R] = ??? //running the coroutine until the first yield occurence
  def resume: Boolean = ??? //are we blocked by a yield?
  def value: B = ??? 
}

object Coroutine {
  def apply[A, B, R](f: A => R): Coroutine[A, B, R] = new Coroutine[A,B,R](f)
  def yieldval[B](value: B): Unit = ???
}

object Example1 {

  import Coroutine._
  //Idea taken from page 5 of "Revisiting coroutines"
  def printAtSamePosition[A, B](ls1: List[A], ls2: List[B]) {

    def getNext[T] = Coroutine[List[T], T, Unit]{ (ls : List[T]) => {
      ls foreach yieldval
    }}

    val iterator1 = getNext[A].call(ls1)
    val iterator2 = getNext[B].call(ls2)

    //traversing both collections until one has been entirely traversed
    while (iterator1.resume && iterator2.resume) {
      print(iterator1.value +", "+ iterator2.value)
    }
  
  }
}
