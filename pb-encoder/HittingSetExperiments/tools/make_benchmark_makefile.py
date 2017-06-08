#!/usr/bin/env python2
import sys, os, random, itertools
from config import toolBenchmarkPairs, TIME_LIMIT_IN_SECONDS, MEMORY_LIMIT_IN_KB

# Get own prefix
ownProgramPrefix = sys.argv[0]
assert ownProgramPrefix.endswith("make_benchmark_makefile.py")
ownProgramPrefix = ownProgramPrefix[0:len(ownProgramPrefix)-26]

# Enumerate all tasks
tasks = []
targets = []

# Build combinations
for (files,tools) in toolBenchmarkPairs:
    for tool in tools:
        if os.path.exists("../lib/"+tool):
            for myfile in files:
                targets.append(".res/"+myfile+"."+tool)
                tasks.append(".res/"+myfile+"."+tool+": "+myfile+"\n")
                tasks.append("\tmkdir -p .res\n")
                tasks.append("\t"+ownProgramPrefix+"../lib/timeout -m "+str(MEMORY_LIMIT_IN_KB)+" -t "+str(TIME_LIMIT_IN_SECONDS)+" \"("+ownProgramPrefix+"../lib/"+tool+" "+myfile+" > .res/"+myfile+"."+tool+"; test $$? -eq 10 -o $$? -eq 0)\" 2>.res/"+myfile+"."+tool+".stderr\n\n")
        else:
            print "Note: Tool "+tool+" not found, thus skipping it. See README.md for hints."
            
with open("Makefile","w") as makefile:
    print >>makefile,"default :",
    for a in targets:
        print >>makefile,a,
    print >>makefile,"\n\t@echo Done!\n\n"
    
    for a in tasks:
        makefile.write(a)
