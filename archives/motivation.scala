object AlternativeDesign {
  //def yieldval[T](t: T)(implicit dummyValueFromCoroutine: ..): Unit = ???
  def yieldval[T](t: T): Unit = ???

  def coroutine[T](body: => Unit): Coroutine[T] = ???
  abstract class Coroutine[+T] {

    def run(f: T => Unit): Unit = {
        var res = this.continue
        while (res.nonEmpty) {
          f(res.get)
          res = this.continue
        }
    }
    def continue: Option[T] 
  }


  val co = coroutine[Int] {
    print("hello world")
    yieldval(1+1)
    yieldval(2*2)
  }

  //translates at the definition site to
  new Coroutine[Int] {
    var state: Int = 0
    def continue: Option[Int] = state match {
      case 0 => f0
      case 1 => f1 
      case 2 => f2
    }

    private def f0 = {print("hello world");  state =1; Some(1+1)}
    private def f1 = {state = 2; Some(2*2)}
    private def f2 = {state = -1; None}
    
    def isDone: Boolean = state == -1
  }    


}
//TODO justify the design with a paragraph of explanation
//implement coroutine with macro. 
//Suppose we only have block, lit and function
//what if there is if condition and while condition in the code?





//- Q1 What if a coroutine takes multiple parameters?
// Is there a common type to Function types? If this common type is Fun
// the api could look like:
class CoroutineInstance[Y, R] {
  //TODO resume taking a parameter (seek examples), returns an option
  def resume: Boolean = ??? //returns true if there is still values to be yielded.
  def value: Y = ???  // returns the value to be yielded (to be called after resume).
  def isDone: Boolean = ??? //did we go after all yields?
  def result: R = ??? //the result of the function

  /* run takes a function which uses the value returned by the yield
  * The function yieldValueUser will be called as long as this.resume() returns true.
  */
  def run(yieldValueUser: Y => Unit): Unit = ???
  //def step(Option[] => )
}

//use cases of step where it is used like this:
//m.step {
//  case Some(...) => ...
//  case None => 
//}  

class Coroutine0[Y, R](val f: () => R) {
  def call(): CoroutineInstance[Y, R] = ???
}
class Coroutine1[I0, Y, R](val f: I0 => R) {
  def call(input0: I0): CoroutineInstance[Y, R] = ???
}

class Coroutine2[I0, I1, Y, R](val f: (I0, I1) => R) {
  def call(input0: I0, input1: I1): CoroutineInstance[Y, R] = ???
}
// ...

/*maybe there is a way to extract the input type of the function given 
 to the Coroutine class constructor with macros?

class CoroutineGeneric[Y, R](val f: GenericFunctionType with return type R) {
  def call(inputs: macro getting input type of f): CoroutineInstance[Y, R]
}
*/

/*
- Q2 Can the parameters be captured from the enclosing environment?
- Q3 Why we need a `call` method on coroutine?
*/
object ParamsFromEnv {
  import Coroutine._
  def reverseList() {
    val lsFromEnv = List(1, 2, 3)

    //TODO we need statistics about call
    //Q3 here call is pretty unnecessary..
    val coroutine = new Coroutine0[Int, Unit] ( () => lsFromEnv foreach yieldval).call

    var allYields = List[Int]()
    while (coroutine.resume) {
      allYields = coroutine.value :: allYields
    }
  }
}

/*


Conceptual question:

- Q5 Is the design easily extensible to stackful coroutines?

from revisiting coroutines 3.1:
The ﬁrst time a coroutine is activated, a second argument given to the operator
resume is passed as an argument to the coroutine main function. In subse-
quent reactivations of a coroutine, this second argument becomes the result
value of the operator yield

so that means that we have to pass the result of yield back to coroutine.resume each time we call it



We need a semantically different keyword when we call a coroutine inside
a coroutine.

*/

class Coroutine[A, Y, R](val f: A => R) {
  def call(input: A): Coroutine[A, Y, R] = ??? //running the coroutine until the first yield occurence
  def resume: Boolean = ??? //returns true if there is still values to be yielded.
  def value: Y = ???  // returns the value to be yielded (to be called after resume).
}

object Coroutine {
  def apply[A, Y, R](f: A => R): Coroutine[A, Y, R] = new Coroutine[A,Y,R](f)
  def yieldval[Y](value: Y): Unit = ???
}

object TwoListsIteration {

  import Coroutine._
  //Idea taken from page 5 of "Revisiting coroutines"
  def printAtSamePosition[A, B](ls1: List[A], ls2: List[B]) {

    def getNext[T] = Coroutine[List[T], T, Unit]{ (ls : List[T]) => {
      ls foreach yieldval
    }}

    val iterator1 = getNext[A].call(ls1) // Q3 call can be useful sometimes though
    val iterator2 = getNext[B].call(ls2)

    //traversing both collections until one has been entirely traversed
    while (iterator1.resume && iterator2.resume) {
      print(iterator1.value +", "+ iterator2.value)
    }
  
  }
}

// - Q4 Do we really need a return value for a coroutine? At least in the example above, it seems we don't care about the result, only the yielded value.
//example of storm en route about async await seems to say it is useful.
//
//following example is made up
object IterateAndSum {
  import Coroutine._
  def example() {
    val ls = List(1,2,3,4).iterator
    new Coroutine0[Int, Int](() => {
      var sum = 0
      while (ls.hasNext) {
        val cur = ls.next()
        sum = sum + cur
        yieldval(cur)
      }

      sum
    }).call
    
  }

}

// http://storm-enroute.com/coroutines/docs/0.6/example-async-await/index.html
// 
//import scala.annotation.unchecked.uncheckedVariance
//import scala.concurrent._
//import scala.concurrent.duration._
//import scala.concurrent.ExecutionContext.Implicits.global
// 
// 
// 
//object AsyncAwait {
//  import Coroutine._
//  class Cell[+T] {
//    var x: T @uncheckedVariance = _
//  }
// 
//  /** The future should be computed after the pair is yielded. The result of
//   *  this future can be used to assign a value to `cell.x`.
//   *  Note that `Cell` is used in order to give users the option to not directly
//   *  return the result of the future.
//   */
//  def await[R]: Coroutine1[Future[R], (Future[R], Cell[R]), R] =
//    new Coroutine1 ( (f: Future[R]) => {
//      val cell = new Cell[R]
//      yieldval((f, cell))
//      cell.x
//    })
// 
//  def async[Y, R](body: Coroutine0[(Future[Y], Cell[Y]), R]): Future[R] = {
//    val c = body.call
//    val p = Promise[R]
//    def loop() {
//      if (!c.resume) p.success(c.result)
//      else {
//        val (future, cell) = c.value
//        for (x <- future) {
//          cell.x = x
//          loop()
//        }
//      }
//    }
//    Future { loop() }
//    p.future
//  }
// 
//  def main(args: Array[String]) {
//    val f = Future { math.sqrt(121) }
//    val g = Future { math.abs(-15) }
//    /** Calls to yieldval inside an inner coroutine are yield points inside the
//     *  outer coroutine.
//     */
//    val h = async(new Coroutine0[(Future[Int], Cell[Int]), Int]( () => {
//      val x = await.call(f)
//      val y = await.call(g)
//
//      x + y
//    }))
// 
//    val res = scala.concurrent.Await.result(h, 5.seconds)
//    assert(res == 26.0)
//  }
//}
//
//================================================================================



sealed abstract class BinaryTree[+T] {
  def isEmpty: Boolean 
}

case class Node[T](val left: BinaryTree[T], val value: T, val right: BinaryTree[T]) extends BinaryTree[T] {
  def isEmpty: Boolean = false
}

case object Leaf extends BinaryTree[Nothing] {
  def isEmpty: Boolean = true 
}

//taken from page 11 of "revisiting coroutines"
object BinaryTreeIterator {
  import Coroutine._

  def inorder[T](tree: BinaryTree[T]): Unit = tree match {
    case Node(l, v, r) => 
      inorder(l) 
      yieldval(v)
      inorder(r)

    case Leaf => ;
  }

  def convertTreeToList[T](tree: BinaryTree[T]): List[T] = {
    val coroutine = Coroutine[BinaryTree[T], T, Unit] { t => inorder(t) }.call(tree)

    var ls: List[T] = List.empty[T]
    while (coroutine.resume) {
      ls = coroutine.value :: ls
    }

    ls
  }

  def simpleTest() { //to run later
    val tree = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf))
    assert(convertTreeToList(tree) == List(1, 2, 3))
  }
}


//translation of code from page 14 of revisiting coroutines
object PatternMatching {
  import Coroutine._

  type Pattern = (String, Int) => Unit
  def prim(str: String): Pattern = (s: String, pos: Int) => {
    val len = str.length
    if (s.substring(pos, pos+len) == str) {
      yieldval(pos+len)
    }
  }

  def alt(patt1: Pattern, patt2: Pattern): Pattern = (s: String, pos: Int) => {
    patt1(s, pos)
    patt2(s, pos)
  }

  def seq(patt1: Pattern, patt2: Pattern): Pattern = (s: String, pos: Int) => {
    val btpoint = new Coroutine0[Int, Unit](() => patt1(s, pos)).call
    while (btpoint.resume) {
      val npos: Int = btpoint.value
      patt2(s, npos)
    }
  }

  def matchPatt(s: String, patt: Pattern): Boolean = {
    val len = s.length
    val m = new Coroutine0[Int, Unit](() => patt(s,1)).call
    while (m.resume) {
      val pos: Int = m.value
      if (pos == len + 1)
        return true 
    }

    //TODO  we can rewrite as
    m.run((pos: Int) => {
      if (pos == len + 1)
        return true
    })
    //alternatively can create a step function


    false 
  }


  def test {
    //construction of the pattern ("abc" | "de")."x"
    val patt = seq(alt(prim("abc"), prim("de")), prim("x"))
  }
}
