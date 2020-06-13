package coroutines.bench

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
class EmptyBenchJMH  {

//   val fibSizes = 5000 to 25000 by 5000; 

//   val taylorSizes = 50000 to 250000 by 50000
  
  //TODO to avoid dead code use blackholes and return values in benchmarks check out also how to handle loops
  // @gen("fibSizes")
  // @benchmark("coroutines.stream.fibonacci.to-buffer")
  // @curve("stream")
  @Benchmark
  def test = {
      1
  }

}