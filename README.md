# Open-VRP

## Synopsis

Open VRP is a framework to model and solve [VRP-like](http://neo.lcc.uma.es/radi-aeb/WebVRP/) problems for students, academics, businesses and hobbyist alike. This framework allows for quick implementation of simple TSP/VRP problems to more complicated VRPTW, PDPTW, MDCPVRPPDTW, or however cool you want to sound. The library is extensibly written in Common Lisp's CLOS. Depending on your interest/purpose, an algorithm can be:

* written from scratch
* build using the toolkit of shared operators
* or tweaked from existing implementations - (currently only Tabu Search implemented)

The Problem object (e.g. VRP) and the Algorithm object (e.g. Genetic Algorithm) are modelled seperately and combined with the generic method (solve-prob problem algo). Different solution algorithms can be tested and compared against each other on the same problem (which you only model once).

The solutions are drawn using [vecto](http://www.xach.com/lisp/vecto/) in a .png file.

## Vision

Too often have I found myself having to build a VRP model from scratch, just to experiment with some meta-heuristics for a school paper. Academics/students with a background/interest in Mathematics/Operations Research without the skills/patience for die-hard coding (in C++/Java), have no choice but to spend their valuable time stuck in the debug/test/debug cycle. [Here](https://kuomarc.wordpress.com/2012/01/27/why-i-love-common-lisp-and-hate-java/) is why those in OR should consider Common Lisp as an option.

With this framework, I hope to catalyze the research and application of routing solutions. Researchers in innovative new algorithms should not need to fiddle in the Eclipse debugger screen, frustratingly looking for a missing semi-colon. They should be able to focus all their energy and effort in devising their heuristics. OR should be kept fun and clean.

The ultimate vision for Open VRP is a simple intuitive embedded language for the OR community, free for anyone.

## Overview

![alt Open-VRP Class-diagram](https://github.com/mck-/Open-VRP/blob/master/class-diagram.png?raw=true "Open-VRP Class-diagram")

---

![alt Iterator](https://github.com/mck-/Open-VRP/blob/master/iterator.png?raw=true "Iterator")

## Usage

`solve-plot` expects a problem object and an algo object. It calls `solve-prob` and `plot-solution`.

`test-vrp`, `solomon25` and `solomon100` are pre-loaded demo problems. To use Tabu Search:

```
(solve-plot test-vrp (make-instance 'tabu-search :animate T))
(solve-plot solomon100 (make-instance 'tabu-search :iterations 100 :runs 10))
```

When :animate is set to T, each iteration will produce a plot in run-frames/Iteration x.png (much slower). 
Tabu-search supports a simple multi-start heuristic with the keyword :runs. In the above example, we will solve the problem 10 times with 100 iterations and return the best solution.

You can define your own problem objects with:

```
(define-problem name node-coords n "plots/vrp.png")
(define-problem name node-coords n "plots/cvrp.png" demands-list capacity)
(define-problem name node-coords n "plots/vrptw.png" demands-list capacity time-windows durations)
```

where *node-coords* is a list of pairs, *demands-list* a list of associated demands, and n is the number of vehicles. When a *demands-list* and vehicle *capacity* are provided, the resulting problem is a CVRP. If in addition *time-windows* (list of pairs) and *durations* are given, the resulting problem object is a VRPTW.

Or to load from a text-file [Solomon-format](http://neo.lcc.uma.es/radi-aeb/WebVRP/index.html?/Problem_Instances/CVRPTWInstances.html):

```
(defvar test-case (load-testcase-solomon "path-to-file.txt"))
(solve-plot test-case (make-instance 'tabu-search :iterations 100))
```

When the algo is finished running, it returns the Algo object, which contains :current-sol and :best-sol. Use `iterate-more` to keep searching:

```
(iterate-more <algo> int)
```

## Output

An example output of Solomon's VRPTW 100-customers benchmark test-case, solved with Tabu Search.

![alt Optimal solution](https://github.com/mck-/Open-VRP/blob/master/plots/solomon100-optimal.png?raw=true "Optimal solution")

Currently, the output is in the REPL, except for the plot. Log stats is **wip**.

## TODO

* Extend VRP model to PDPTW
* Run logs/statistics for test-result gathering (including batch runs)
* User-interface (better macros)
* Plotting can be improved (real-time output instead of .png files)
* ...

## License

Open-VRP is licensed under the terms of the [Lisp Lesser GNU
Public License](http://opensource.franz.com/preamble.html), known as
the LLGPL.  The LLGPL consists of a preamble (see above URL) and the
LGPL.  Where these conflict, the preamble takes precedence. 
Open-VRP is referenced in the preamble as the "LIBRARY."
