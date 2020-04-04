package coroutines

//def yieldval[T](t: T)(implicit dummyValueFromCoroutine: ..): Unit = ???


//TODO put this in different package that is for library code and create junit tests.


 
def yieldval[T](t: T): T = t

abstract class Coroutine[+T] {
  
  def run(f: T => Unit): Unit = {
    var res = this.continue
    while (res.nonEmpty) {
      f(res.get)
      res = this.continue
    }
  }

  def continue: Option[T] 

  //TODO why I cannot use protected here without making stuff in Macros.scala crash?
  var state: Int = 0

}

object Test {   
  import Macros._
    //Suppose we only have block, lit and function
    //what if there is if condition and while condition in the code?



    def test = { 
      val co = coroutine[Int]({
        print("hello world")
        yieldval(1+1)
        yieldval(2*2)
      })

      //should translates at the definition site to
      val objective = new Coroutine[Int] {
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
}




