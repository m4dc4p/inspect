# Setup (Scala)

To run this application, make sure you have Java installed (at least JDK 1.8/Java 8).

Then, download & install SBT (https://scala-sbt.org)

Once `sbt` is installed, run it in this directory:

```bash
$ sbt
```

At the prompt, enter the `run` command, which will start a server on port 9000:

```bash
sbt> run
[info] running com.example.echo.Main
23:32:25.857 [ioapp-compute-0] INFO org.http4s.blaze.channel.nio1.NIO1SocketServerGroup - Service bound to address /0:0:0:0:0:0:0:0:9000
23:32:25.866 [blaze-selector-0] DEBUG org.http4s.blaze.channel.nio1.SelectorLoop - Channel initialized.
23:32:25.870 [ioapp-compute-0] INFO org.http4s.server.blaze.BlazeServerBuilder -
  _   _   _        _ _
 | |_| |_| |_ _ __| | | ___
 | ' \  _|  _| '_ \_  _(_-<
 |_||_\__|\__| .__/ |_|/__/
             |_|
23:32:25.938 [ioapp-compute-0] INFO org.http4s.server.blaze.BlazeServerBuilder - http4s v1.0.0-M5 on blaze v0.14.13 started at http://[::]:9000/
```

# Setup (Haskell)

Use cabal to run the sample application:

```bash
$ cabal run
```

# Implementation Considerations

The problem presented (storing counts of keys seen & returning those counts) invites a number of implementation choices, depending
on the expected access patterns and the capabilities we want from the service. Below I discuss some possibilities for implementing 
the service entirely locally or via a distributed system.

# Local Solutions

An in-memory solution, where keys & counts are stored locally, works well when the number of keys stored (or the counts) is
not expected to grow beyond 10s of GBs. At some point, the cost of instance with a larger & larger memory capacity exceeds the
inconvience of supporting networke solutions, but if that point is not expected to be reached then an in-memory solution is easier
to implement & maintain.

## Tree Structures

If we can assume that the keys stored tend to share a prefix, I'd consider a tree structure, especially if they keys were sparsely distributed. The downside here is that as the number of unique keys grows, so does the size of the data stored.

Conversely, if the keys will be densely packed (i.e., we are likely to see almost all possible keys) and they are small enough , we could use a tree struture such as a array-backed binary tree. That would use an amount of memory proportional to the size  of the key space, but if we knew all those keys were likely to be seen then that isn't a bad trade-off.

## Hash Tables

If the keys will be randomly distributed, then I'd consider a hash-based structure. This allows efficient use of memory compared to trees, and supports fast lookup & insertion. 
For the problem specified, given there aren't any constraints, I think this structure would be the best for in-memory solutions.

## Probabilistic Structures

Since Bloom filters came up in our discussions, I'd like to address them. Bloom filters will always tell you if a key is not present, but won't tell you if the key is definitely
presentr. Bloom filters (classically) also don't store any information about the elements stored - only if they are present or not. In our case, the count would still need
to be stored in another data structure. 

The utility of using a Bloom filter depends on the keys we expect, and how expensive double-checking for presence is. If most keys will be unique, then the filter will let us record new keys easily. If we tend to see keys over and over, and especially if looking up an existing key is expensive, then the Bloom
filter won't be that useful. 

However, if we are very concerned about memory usage, then the bloom filter does makes sense, even if lookup is expensive.

# Networked Services

If we expect the number of keys inserted or queried to be very large, or if we want to ensure we can horizontally scale, then it makes sense to solve this problem using
a distributed system. In this case, we also need to decide how consistent we want the lookups to be - is it ok if a few counts are stale, or do they always need to
be accurate? Below, I consider three distributed solutions: Postgres-based upserts, memcached, and redis.

Which choice to make depends on how we expect the service to grow (features), what kind of access patterns we expect, and any SLAs imposed by clients.

## Postgres

Postgres supports some very fast patterns for inserting or updating keys (upserts). I'm not sure the exact point postgres stops making sense (I think for very write-heavy workloads
its not appropriate), but  I would  expect Postgres to be good choice except for the heaviest workloads.

## Memcached/Postgres

If Postgres alone wasn't enough, and the workload tended towards reading rather than writing, I would consider using memcached as a write-through cache, along with 
Postgres. We'd still be limited by Postgres' write performance, but reads would be as fast as the cache could serve them. 

## Memcached/NoSQL

If the workload was very write-heavy, and counts could be a little inaccurate, I would consider using a distributed NoSQL solutions such as Cassandra (or a similar system). 
Cassandra can write quickly when the quorum required is low (because each write doesn't wait for replication). That could result in lost writes and counts, which may
not be acceptable. If writes cannot be lost, NoSQL may not be the best solution. 

## Redis

If the workload was expected to be write & ready heavy, I would likely reach for Redis. As Redis natively supports the ability to increment a count associated with
a key, it would solve our problem immediately. I don't have operational experience with Redis, and would need to ensure it could provide the performance required.