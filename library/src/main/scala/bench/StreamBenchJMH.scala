package coroutines.bench

import coroutines._ 
import coroutines.Macros._

//for benchmarking
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection._
import scala.language.implicitConversions




@State(Scope.Benchmark) //All threads running the benchmark share the same state object.
@Warmup(iterations = 50)    // translation of "exec.minWarmupRuns", 50 ; "exec.maxWarmupRuns", 100 
@BenchmarkMode(Array(Mode.All))
@Measurement(iterations = 36) //"exec.benchRuns", 36 
@Fork(value = 4) //"exec.independentSamples", 4 ?
class StreamBenchJMH  {

  @Param(Array("5000", "10000", "15000", "20000", "25000"))
  var fibSize: Int = _

  @Param(Array("50000", "100000", "150000", "200000", "250000"))
  var taylorSize: Int = _
  
 
  //For now this doesnt work due to this issue https://github.com/ktoso/sbt-jmh/pull/178
  // @Benchmark
  // def streamFibonacciToBuffer(bh: Blackhole) = {
  //   val sz = fibSize
  //   val buffer = mutable.Seq[BigInt]() //TODO come back to  a mutable buffer
  //   object Fibs {
  //     lazy val values: Stream[BigInt] =
  //       BigInt(0) #:: BigInt(1) #:: values.zip(values.tail).map(t => t._1 + t._2)
  //   }
  //   var i = 0
  //   var s = Fibs.values
  //   while (i < sz) {
  //     buffer :+ s.head
  //     s = s.tail
  //     i += 1
  //   }
  //   bh.consume(buffer)
  // }



  //For now this doesnt work due to this issue https://github.com/ktoso/sbt-jmh/pull/178
  // @Benchmark
  // def coroutineFibonacciToBuffer(bh: Blackhole) = {
  //   val sz = fibSize
  //   var buffer: Seq[BigInt] = Seq() //TODO come back to a mutable buffer
  //   val fibs = coroutine[BigInt] { 
  //     var prev = BigInt(0)
  //     var curr = BigInt(1)
  //     yieldval(prev)
  //     yieldval(curr)
  //     while (true) {
  //       val x = curr + prev
  //       yieldval(x)
  //       prev = curr
  //       curr = x
  //     }
  //   }
  //   var i = 0
  //   while (i < sz) {
  //     fibs.continue().foreach { x => buffer = buffer :+ x }
  //     i += 1
  //   }
  //   bh.consume(buffer)
  // }

  // @gen("taylorSizes")
  // @benchmark("coroutines.stream.taylor.sum")
  // @curve("stream")
  @Benchmark
  def streamTaylorSum = {
    val sz = taylorSize
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
  def coroutineTaylorSum = {
    val sz = taylorSize
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