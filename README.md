# Open-VRP

Check out the [Wiki](https://github.com/mck-/Open-VRP/wiki) for an overview of Open-VRP or scroll down for a summary, [fork](https://github.com/mck-/Open-VRP/fork_select) and get-started!

## Synopsis

Open VRP is a framework to model and solve [VRP-like](http://neo.lcc.uma.es/radi-aeb/WebVRP/) problems for students, academics, businesses and hobbyist alike. This framework allows for quick implementation of simple TSP/VRP problems to more complicated VRPTW, PDPTW, MDCPVRPPDTW, or however cool you want to sound. The library is extensibly written in Common Lisp's CLOS. Depending on your interest/purpose, an algorithm can be:

* [written from scratch](https://github.com/mck-/Open-VRP/wiki/Using-Open-VRP:-writing-your-algo-from-scratch)
* tweaked from existing implementations - (currently only [Tabu Search implemented](https://github.com/mck-/Open-VRP/wiki/Description-of-the-Tabu-Search-implementation))

The Problem object (e.g. VRP) and the Algorithm object (e.g. Genetic Algorithm) are modelled seperately and combined with the generic method (solve-prob problem algo). Different solution algorithms can be tested and compared against each other on the same problem (which you only model once).

## Current features (v. 0.6.2)

* TSP, VRP, CVRP, VRPTW, CVRPTW
* Homogenous/heterogenous fleet
* Demands, duration, capacity, time-windows, speed
* Define network using coordinates or (asymettric) distance matrix
* Tabu Search
* Logging of search progress (to file or to REPL)
* Plotting of final solution or after each iteration
* Test-case loader (Solomon and TSPLIB format)
* Batch-run to test algo on a directory of test-cases

## Vision

Too often have I found myself having to build a VRP model from scratch, just to experiment with some meta-heuristics for a school paper. Academics/students with a background/interest in Mathematics/Operations Research without the skills/patience for die-hard coding (in C++/Java), have no choice but to spend their valuable time stuck in the debug/test/debug cycle. [Here](https://kuomarc.wordpress.com/2012/01/27/why-i-love-common-lisp-and-hate-java/) is [why](http://kuomarc.wordpress.com/2012/03/05/the-uncommon-lisp-approach-to-operations-research/) those in OR should consider Common Lisp as an option.

With this framework, I hope to catalyze the research and application of routing solutions. Researchers in innovative new algorithms should not need to fiddle in the Eclipse debugger screen. They should be able to focus all their energy and effort in devising their heuristics. OR should be kept fun and engaging.

The ultimate vision for Open VRP is a simple intuitive toolkit for the OR community, free for anyone.

## Installation

```
~$ git clone git://github.com/mck-/Open-VRP.git
```
Add this path and evaluate require:

```
(push "/path/to/Open-VRP/" asdf:*central-registry*)
(require 'open-vrp)
(in-package :open-vrp)
```

## Usage
Check the [Wiki](https://github.com/mck-/Open-VRP/wiki) for more documentation, the following is a short summary of the main functionality.

`solve-prob` expects a problem object and an algo object.

`test-vrp`, `solomon25`, `solomon100`, `christofides-1` and `christofides-2` are pre-loaded demo problems. To use Tabu Search:

```
(solve-prob test-vrp (make-instance 'tabu-search :iterations 10 :animatep T))
(solve-prob solomon100 (make-instance 'tabu-search :iterations 100))
(solve-prob christofides-2 (make-instance 'tabu-search :iterations 50))
```

By default, problems will plot to `plots/name.png` and log to `run-logs/name.txt` where _name_ refers to the `:name` slot of the _Problem_ object. `(toggle-plot <problem>)` to disable plotting the final solution. Use `(set-log-mode <problem> x)` to switch from [0] no logging, [1] logging to file or [2] logging to the REPL. 

If you don't like the legend in the plot, turn it off with `(toggle-legend <problem>)`.

When :animatep is set to T, each iteration will produce a plot in run-frames/Iteration x.png (much slower, since it needs to plot each iteration). You may use `(toggle-animate <algo>)` to turn it on/off.

You can define your own problem objects with:

```
(define-problem "VRP" fleet-size :node-coords-list node-coords :to-depot T)
(define-problem "CVRP" fleet-size :node-coords-list node-coords :demands demands-list :capacities capacity-list)
(define-problem "VRPTW" fleet-size :node-coords-list node-coords :time-windows time-windows :durations durations :speeds speed)
```

where *node-coords* is a list of pairs, *demands-list* a list of associated demands (must be same length), and *fleet-size* is the number of vehicles. When a *demands-list* and vehicle *capacity* are provided, the resulting problem is a CVRP. If *time-windows* (list of pairs) and *durations* are given, the resulting problem object is a VRPTW. When everything is provided, it creates a CVRPTW. Each class of problem has its own specific constraints to check.

You may also provide a(n asymmetric) distance matrix instead of node-coords (real-life problems). You won't be able to plot the solution without *node-coords* though.

```
(define-problem "ASYM-CVRP" 3 :demands '(0 1 2 3 2 1) :capacities 8 :dist-array dist-aray)
```
Note that the above will create 6 nodes, so the dimensions of the dist-array must be 6x6. Also note that we provide a single number for capacities (instead of a list with length 3), which means that all vehicles will have a capacity of 8. Single numbers are allowed for :demands, :durations, :capacites and :speeds

Or to load a problem from a text-file (currently supports [Solomon-format](http://neo.lcc.uma.es/radi-aeb/WebVRP/index.html?/Problem_Instances/CVRPTWInstances.html) and [TSPLIB cvrp format](http://neo.lcc.uma.es/radi-aeb/WebVRP/data/Doc.ps)):

```
(defvar test-case-solomon (load-test-case-file "path-to-solomon-file-format.txt"))
(defvar test-case-tsplib (load-test-case-file "path-to-tsplib-file-format.vrp"))
```

When the algo is finished running, it returns the Algo object, which contains :current-sol and :best-sol. Use `iterate-more` to keep searching:

```
(iterate-more <algo> int)
```

## Output

An example output of Solomon's VRPTW 100-customers benchmark test-case, solved with Tabu Search.

![alt Optimal solution](https://github.com/mck-/Open-VRP/blob/master/plots/solomon100-optimal.png?raw=true "Optimal solution")

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
