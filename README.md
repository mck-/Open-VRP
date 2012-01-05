# Open-VRP

## Synopsis

Open VRP is a framework to model and solve [VRP-like](http://neo.lcc.uma.es/radi-aeb/WebVRP/) problems for students, academics, businesses and hobbyist alike. This framework allows for quick implementation of simple TSP/VRP problems to more complicated VRPTW, PDPTW, MDCPVRPPDTW, or however cool you want to sound. The library is extensibly written in Common Lisp's CLOS. Depending on your interest/purpose, an algorithm can be:

* written from scratch
* build using the toolkit of shared operators
* or tweaked from existing implementations **(wip)**

The Problem object (e.g. VRP) and the Algorithm object (e.g. Genetic Algorithm) are modelled seperately and combined with the generic method (solve-prob problem algo). Different solution algorithms can be tested and compared against each other on the same problem (which you only model once).

The solutions are drawn using [vecto](http://www.xach.com/lisp/vecto/) in a .png file. Logs and statistics of the run are summarised in a .txt file **(wip)**.

## Usage
   
```
(load "load-all.lisp")
```
Includes loading test-data.lisp and test-data-init.lisp for demo:

```
(solve-plot test-tsp (make-instance 'greedy-NN))
```
or

```
(solve-plot test-vrp (make-instance 'greedy-insertion))
```

or using Tabu Search (animate plots every iteration in a .png file in run-frames/)

```
(solve-plot test-vrp (make-instance 'tabu-search :iterations 5 :animate T))
```

You can define your own problems with (to be extended)

```
(define-problem 'vrp *node-coords* n "output.png")
```
where *node-coords* is a list of node-coords and n is the number of vehicles.


## Vision

Too often have I found myself having to build a VRP model from scratch, just to experiment with some meta-heuristics for a school paper. Academics/students with a background/interest in Mathematics/Operations Research WITHOUT the skills/passion for coding, have no choice but to spend their valuable time stuck in the debug/test/debug cycle.

With this framework, I hope to catalyze the research and application of routing solutions. Researchers in innovative new algorithms should not need to fiddle in the Eclipse debugger screen, frustratingly looking for a missing semi-colon. They should be able to focus all their energy and effort in devising their heuristics. OR should be kept fun and clean.

The ultimate vision for Open VRP is a simple intuitive embedded language for the OR community, free for anyone.

## TODO

* A search framework for meta-heuristics (Tabu Search, Genetic Algorithms, etc..) **(wip)**
* Benchmark test-case loader (Taillard/Solomon/Li&Lim, etc...)
* Extend VRP model to CVRP, VRPTW and PDPTW
* Run logs/statistics for test-result gathering (including batch runs)
* User-interface (better macros)

## License

Open-VRP is licensed under the terms of the [Lisp Lesser GNU
Public License](http://opensource.franz.com/preamble.html), known as
the LLGPL.  The LLGPL consists of a preamble (see above URL) and the
LGPL.  Where these conflict, the preamble takes precedence. 
Open-VRP is referenced in the preamble as the "LIBRARY."
