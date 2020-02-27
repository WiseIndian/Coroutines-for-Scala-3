class Coroutine[A, B, R](val f: A => R) {
  def call(input: A): Coroutine[A, B, R] = ??? //running the coroutine until the first yield occurence
  def resume: Boolean = ??? //returns true if there is still values to be yielded.
  def value: B = ???  // returns the value to be yielded (to be called after resume).
}

object Coroutine {
  def apply[A, B, R](f: A => R): Coroutine[A, B, R] = new Coroutine[A,B,R](f)
  def yieldval[B](value: B): Unit = ???
}

object TwoListsIteration {

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
