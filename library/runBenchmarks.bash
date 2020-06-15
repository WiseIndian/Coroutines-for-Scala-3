#this will run jmh benchmarks
sbt 'jmh:run -rf json -rff fibo.json .*FibonacciBenchJMH.* -jvmArgs "-Xms1G -Xmx1G"' 2>&1 > fibo.out
echo done with fibo

sbt 'jmh:run -rf json -rff taylor.json  .*TaylorBenchJMH.* -jvmArgs "-Xms1G -Xmx1G"' 2>&1 > taylor.out
echo done with taylor

sbt 'jmh:run -rf json -rff tree.json  .*TreeIteratorBenchJMH.* -jvmArgs "-Xms1G -Xmx1G"' 2>&1 > tree.out

echo done with tree
