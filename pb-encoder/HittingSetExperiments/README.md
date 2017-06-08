Some experiments for encoding the hitting set problem into SAT and SMT.

Installation:

1. Install "pblib" from TU Dresden into the lib/pblib directory
2. Ensure that you have a moderately modern version of Python2 installed
3. Go to the "tools/cardinalityConstraintSATEncoder" directory and run "qmake Tool.pro" and then "make"
4. Install the memtimeout tool
5. Add links with "ln -s" to "picosat", "minisat", "lingeling", "z3" to the "lib" directory
6. Go to the benchmark directory and run "../tools/benchmarkEncoder.py <inputFile>" on every benchmark ".txt" file that you are interested in.
7. Run "../tools/make_benchmark_makefile.py" to generate a benchmarking Makefile
8. Run "make -j <numberOfProcessorsToUse>"
9. Run "../tools/make_results_summary.py > results.tex"
10. Run "pdflatex results.tex" two times to get results.
