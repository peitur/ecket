
<h1>
Concurrent Bucket sorting Experiment, in Erlang
</h1>

This is an experiment in implementing and testing a simple somewhat generic concurent b-tree (bucket tree) implementation in Erlang


Buckets are the major containers, they do the high level split of the data into multiple layers based on sorting functions.
To create multilevel buckets, a bucket generator fun is passed to each layer to create the diffrent paralell buckets.


1. External API

2. Buckets

3. Items

Abstraction layer for lists. 
Can be reimplemented with other mem-storage structures.
Needs to be thought through, to match the buckets. 

