import coroutines._ 
import coroutines.Macros._


import org.openjdk.jmh.annotations._
import scala.collection._
import scala.language.implicitConversions




@State(Scope.Thread)
@Warmup(iterations = 50)    // translation of "exec.minWarmupRuns", 50 ; "exec.maxWarmupRuns", 100 
@BenchmarkMode(Array(Mode.All))
@Measurement(iterations = 36) //"exec.benchRuns", 36 
@Fork(value = 4) //"exec.independentSamples", 4 ?
class StreamBenchJMH  {

  val fibSizes = 5000 to 25000 by 5000; 

  val taylorSizes = 50000 to 250000 by 50000
  
  //TODO to avoid dead code use blackholes and return values in benchmarks check out also how to handle loops
  // @gen("fibSizes")
  // @benchmark("coroutines.stream.fibonacci.to-buffer")
  // @curve("stream")
  @Benchmark
  def streamFibonacciToBuffer(sz: Int) = {
    val buffer = mutable.Buffer[BigInt]()
    object Fibs {
      lazy val values: Stream[BigInt] =
        BigInt(0) #:: BigInt(1) #:: values.zip(values.tail).map(t => t._1 + t._2)
    }
    var i = 0
    var s = Fibs.values
    while (i < sz) {
      buffer += s.head
      s = s.tail
      i += 1
    }
    buffer
  }

  // @gen("fibSizes")
  // @benchmark("coroutines.stream.fibonacci.to-buffer")
  // @curve("coroutine")
  @Benchmark
  def coroutineFibonacciToBuffer(sz: Int) = {
    val buffer = mutable.Buffer[BigInt]()
    val fibs = coroutine[BigInt] { 
      var prev = BigInt(0)
      var curr = BigInt(1)
      yieldval(prev)
      yieldval(curr)
      while (true) {
        val x = curr + prev
        yieldval(x)
        prev = curr
        curr = x
      }
    }
    var i = 0
    while (i < sz) {
      fibs.continue().foreach { buffer += _ }
      i += 1
    }
    buffer
  }

  // @gen("taylorSizes")
  // @benchmark("coroutines.stream.taylor.sum")
  // @curve("stream")
  @Benchmark
  def streamTaylorSum(sz: Int) = {
    var sum = 0.0
    class TaylorInvX(x: Double) {
      lazy val values: Stream[Double] =
        1.0 #:: values.map(_ * (x - 1) * -1)
    }
    var i = 0
    var s = new TaylorInvX(0.5).values
    while (i < sz) {
      sum += s.head
      s = s.tail
      i += 1
    }
    sum
  }

  // @gen("taylorSizes")
  // @benchmark("coroutines.stream.taylor.sum")
  // @curve("coroutine")
  @Benchmark
  def coroutineTaylorSum(sz: Int) = {
    var sum = 0.0
    def taylor(x: Double) = coroutine[Double] { 
      var last = 1.0
      yieldval(last)
      while (true) {
        last *= -1.0 * (x - 1)
        yieldval(last)
      }
    }
    var i = 0
    val c = taylor(0.5)
    while (i < sz) {
      c.continue().foreach { sum += _ }
      i += 1
    }
    sum
  }

}