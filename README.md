# Simplication

Simple logic implication visualizer as a communication aide.

## Overview

This is the start of an experiment in logic visualization.

[Try it online](https://simplication.z22.web.core.windows.net/)

The premise is that a simple logical diagram is more effective than natural language when communicating
proposition logic that entails implication, conjunction, and disjunction.

### Understanding Implication

It seems to me that implication is not universally well understood in the office workplace.

- **Engineer** OK. So based on what has been said up to this point: If `A` then `B`.
- **Non Engineer** How can you think that `B` is true? I don't think it is.
- **Engineer** I didn't say anything about the truth of `B` alone; only that if `A` turns out to be true then `B` must also be true based on something we just said. That's important because if `A` turns out to be true then, based on what we said together as a group, `B` must also be true. If it turns out that we learn for a fact that `B` is false then we have a contradiction. This indicates there is an error in our analysis or reasoning; it could just be a lack of precision.
- **Non Engineer** That is all too much detail and very confusing.

### Conjunction Confusion

Conjunctions seem to be better understood, but in my experience the addition of time confuses things.

- **Engineer** If we do `A` and `B` then we can do `C`.
- (...20 minutes elapse...)
- **Non Engineer** OK. So I think we can do `A` and so we can also do `C` based on what engineering has said.
- **Engineer** I'm sorry, but I said that we have to do `A` and `B` to do `C`.

## NPM Package Basics

### Build

```
npm install
./build.sh
```

the above places all assets into /dist.

### Test

```
elm-test
```
