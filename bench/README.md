
# How to run the benchmarks?

`bench/jmh:run -i 10 -wi 10 -f 1 -t max .*Bench.*`
to run all of the benchmarks.

`bench/jmh:run -i 10 -wi 10 -f 1 -t max .*QueueBench.*`
to run only one specific (in this case the `QueueBench`)
benchmark.
