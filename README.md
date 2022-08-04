# Rosen

## Introduction

Rosen-lang is a probabilistic programming language. It allows developers to quickly run queries on discrete probability distributions. Some features are:

- Create a probability distribution with any value type.
- Run a sampling query on the distribution. The sample will be completely random on every execution.
- Run an expectation query on the distribution. The distribution must have values of Int or Real.
- Get a Binomial distribution from a distribution.
- Get a Geometric distribution from a distribution.
- Get the "Join" of distributions, which get a new distribution where every possible arrangement of events is calculated.
- Concat two distributions together.

## Examples
The common data types of the language.
```
>>> 0.5;
0.5
>>> TRUE;
TRUE
>>> 'Heads;
"Heads"
>>> 1;
1
```
To create a distribution you can just specify a mapping. The following is a mapping for a String key.
```
>>> ('heads -> 0.3, 'tails -> 0.7);
| "heads": 0.3
| "tails": 0.7
----
```
You can assign to variables:
```
>>> coin = ('heads -> 0.3, 'tails -> 0.7);
>>> coin;
| "heads": 0.3
| "tails": 0.7
----
```
Note; Rosen is not functional. You are able to reassign variables, and some operations don't act like pure functions (for example, sampling).

To flip a coin 3 times:
```
>>> * (coin, coin, coin);
| ("heads","heads","heads"): 2.7e-2
| ("heads","heads","tails"): 6.3e-2
| ("heads","tails","heads"): 6.3e-2
| ("heads","tails","tails"): 0.14699999999999996
| ("tails","heads","heads"): 6.3e-2
| ("tails","heads","tails"): 0.147
| ("tails","tails","heads"): 0.147
| ("tails","tails","tails"): 0.3429999999999999
```

Suppose we want to see what's the probability of head appearing 4 times if we flip this weighted coin 6 times. This is creating a Binomial distribution from this coin distribution.
```
>>> CREATE BINOMIAL FROM coin WITH ('heads, 6);
| 0: 0.11764899999999995
| 1: 0.3025259999999999
| 2: 0.32413499999999984
| 3: 0.18521999999999988
| 4: 5.9535e-2
| 5: 1.0205999999999998e-2
| 6: 7.289999999999999e-4
```
The parameters let us know that we're looking for a heads to show up, and that we want to flip 6 times.

We can also create a geometric distribution (does not work in all cases) from the coin. This will let us see what's the probability of flipping a coin before getting the first head.
```
>>> CREATE GEOMETRIC FROM coin WITH ('heads);
| *: 2.8247524900000043e-2
| ("heads"): 0.3
| ("tails","heads"): 0.21
| ("tails","tails","heads"): 0.147
| ("tails","tails","tails","heads"): 0.10289999999999999
| ("tails","tails","tails","tails","heads"): 7.202999999999998e-2
| ("tails","tails","tails","tails","tails","heads"): 5.042099999999999e-2
| ("tails","tails","tails","tails","tails","tails","heads"): 3.529469999999999e-2
| ("tails","tails","tails","tails","tails","tails","tails","heads"): 2.4706289999999992e-2
| ("tails","tails","tails","tails","tails","tails","tails","tails","heads"): 1.7294402999999993e-2
| ("tails","tails","tails","tails","tails","tails","tails","tails","tails","heads"): 1.2106082099999995e-2
```
The * here just means that the output was too large to show, so we add up the remaining probabilities and display them as a single entry meaning "everything else". Don't worry though, the distribution itself doesn't get truncated, just the output to the screen. This allows the developer to still use the distribution reliably for calculations. This also enables us to have infinite distributions, like the geometric one.

Now say we want to sample from the coin distribution 10 times for an experiement. This can also be done;
```
>>> CREATE SAMPLE FROM coin WITH (10);
("tails","tails","heads","tails","heads","tails","tails","tails","tails","tails")
>>> CREATE SAMPLE FROM coin WITH (10);
("heads","tails","heads","tails","heads","tails","tails","heads","heads","tails")
>>>
```
As you can see, the sample changes in between calls. Therefore, the developer can always generate more data. 

Now suppose we have a weighted dice, and we want to calculate the expected value;
```
>>> dice = (1 -> 0.1, 2 -> 0.1, 3 -> 0.1, 4 -> 0.1, 5 -> 0.1, 6 -> 0.5);
>>> EXPECT dice;
4.5
```

## Reason for the name
The name was taken from the author of my University textbook for Probability.
