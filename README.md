# ring_calculator

Calculator to find optimal target n-vals for different ring configurations

# Build

```
rebar3 compile
```

# Use

Calculator can either be used as plugin or from Erlang shell.
The idea is that you have a number of nodes to your proposal, think VMs in the cloud, but can also be Erlang nodes on your large machine.
These nodes can be at different locations, where a location is something that could potentially go down with all the nodes on it.

The computed target n-val provides information about fault resistance against failing nodes,
whereas the location n-val provides information about resistance against failing locations.


## Erlang Shell

```bash
$ rebar3 shell
```

In the Erlang shell, use the function
```erlang
-spec nvals(RingSize :: pos_integer(), NrNodes :: pos_integer(), NrLocations :: pos_integer()) -> ok.
```
Where (for riak) the ring size is a power of 2. Typically number of nodes is at least as large as the number of locations.

```
Erlang/OTP 26 [erts-14.2.5] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [jit]

Eshell V14.2.5 (press Ctrl+G to abort, type help(). for help)
1> ring_calculator:nvals(32, 8, 4).
........................................................x................
Best solution:
Target Nval 8
Location val 4
Config [2,2,2,2]
Ring: A1 B1 C1 D1 A2 B2 C2 D2 A1 B1 C1 D1 A2 B2 C2 D2 A1 B1 C1 D1 A2 B2 C2 D2 A1 B1 C1 D1 A2 B2 C2 D2 (0 violations)
ok

2> ring_calculator:nvals(32, 8, 3).
........................................x
Best solution:
Target Nval 8
Location val 2
Config [4,4]
Ring: A1 B1 A2 B2 A3 B3 A4 B4 A1 B1 A2 B2 A3 B3 A4 B4 A1 B1 A2 B2 A3 B3 A4 B4 A1 B1 A2 B2 A3 B3 A4 B4 (0 violations)
ok
```

With 8 nodes and 4 locations at your proposal, you can get a target n-val of 8 and a location nval of 4.
That means that you can configure the system in such a way that you can loose 3 locations or 3 nodes and
still have one copy of all data available. Moreover, you have enough additional nodes to act take over the role of the lost nodes.

With 8 nodes and 3 locations at your proposal, you are unlucky and cannot use the third location effectively.

## Rebar3 plugin

Add the plugin to your rebar config:

```erlang
{project_plugins, [{ring_calculator, {git, "git@github.com:nhs-riak/ring_calculator.git", {branch, "main"}}}]}.
```

Then just call your plugin directly in an existing application:

```bash
$ rebar3 ring_calculator
```

```
Usage: rebar3 ring_calculator [-s <ring_size>] [-n <nodes>]
                              [-l <locations>] [-v <verbose>]

  -s, --ring_size  size of riak ring (default 64)
  -n, --nodes      number of available riak nodes
  -l, --locations  number of physical locations you can run the nodes on
  -v, --verbose    print computation details
```

# Examples

For a riak ring of size 64, we have listed a few solutions for N nodes and L locations:

|nodes|target n-val|locations|location val|
|-----|------------|---------|------------|
|3|2|1|1|
|3|2|2|1|
|3|2|3|2|
|4|4|2|2|
|4|4|3|2|
|4|4|4|4|
|5|4|3|2|
|5|4|4|2|
|5|4|5|4|
|6|5|3|2|
|6|4|4|4|
|7|6|3|2|
|7|5|4|3|
|8|8|2|2|
|8|8|3|2|
|8|8|4|4|

As you can see from that table, for 5 nodes and 4 locations you only get an `location val` of 2.
If you want to increase that value, you can either add one more location, or one more node.
This allows you to cost optimize certain solutions.
