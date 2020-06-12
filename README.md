# coroutines

An implementation of coroutines for Scala 3 based on CPS transformation.

The library directory contains the source code, unit tests and benchmarks.
The archives directory contains outdated code. As well as designs we thought about for the coroutine library initially.

To run the project you can switch to the library directory and run one of the following sbt command:

the unit tests
```
sbt test
```

the main class
```
sbt run 
```

the JMH benchmark
```
jmh:run *BenchMarkClassName*
```
where you have to replace BenchMarkClassName by your choice.
