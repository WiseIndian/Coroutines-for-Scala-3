#this will run jmh benchmarks
sbt 'jmh:run -rf json -rff fibo.json -t 8 .*FibonacciBenchJMH.* -jvmArgs "-Xms1G -Xmx1G"' 2>&1 > fibo.out
echo done with fibo

sbt 'jmh:run -rf json -rff taylor.json -t 8 .*TaylorBenchJMH.* -jvmArgs "-Xms1G -Xmx1G"' 2>&1 > taylor.out
echo done with taylor

sbt 'jmh:run -rf json -rff tree.json -t 8 .*TreeIteratorBenchJMH.* -jvmArgs "-Xms1G -Xmx1G"' 2>&1 > tree.out

echo done with tree
