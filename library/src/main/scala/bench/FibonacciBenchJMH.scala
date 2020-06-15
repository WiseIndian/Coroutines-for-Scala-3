package coroutines.bench

import coroutines._ 
import coroutines.Macros._

//for benchmarking
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection._
import scala.language.implicitConversions




@State(Scope.Thread) //All threads running the benchmark share the same state object.
@Warmup(iterations = 5)    // translation of "exec.minWarmupRuns", 50 ; "exec.maxWarmupRuns", 100 
@BenchmarkMode(Array(Mode.Throughput))
@Measurement(iterations = 10) //"exec.benchRuns"
@Fork(value = 2) //"exec.independentSamples"
class FibonacciBenchJMH  {

  // @Param(Array("5000", "10000", "15000", "20000", "25000"))
  @Param(Array("5000", "15000", "25000"))
  var fibSize: Int = _
 
  @Benchmark
  def streamFibonacciToBuffer(bh: Blackhole) = {
    val sz = fibSize
    object Fibs {
      lazy val values: Stream[BigInt] =
        BigInt(0) #:: BigInt(1) #:: values.zip(values.tail).map(t => t._1 + t._2)
    }
    var s = Fibs.values
    val result = (0 until sz).map {_ =>
      val newVal = s.head
      s = s.tail
      newVal
    }
    bh.consume(result)
  }



  @Benchmark
  def coroutineFibonacciToBuffer(bh: Blackhole) = {
    val sz = fibSize
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
    val result = (0 until sz).flatMap { _ => 
      fibs.continue() //flatmap because this is an option
    }
    assert(result.size > 0)
    bh.consume(result)
  }

 
}