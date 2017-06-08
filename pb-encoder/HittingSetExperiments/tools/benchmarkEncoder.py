#!/usr/bin/env python2
#
# Compile a hitting set example to SMTLIB
#
# ...
import sys, os, math, subprocess

# ==========================================
# Read input file
# ==========================================
if len(sys.argv)<2:
    sys.stderr.write("Error: Need input file name\n")
    sys.exit(1)
with open(sys.argv[1],"r") as inFile:
    allLines = inFile.readlines()

# Prefix for output file names
prefix = sys.argv[1]
assert "." in prefix
while prefix[-1]!=".":
    prefix = prefix[0:len(prefix)-1]
prefix = prefix[0:len(prefix)-1]
    
# Own program prefix
ownProgramPrefix = sys.argv[0]
assert ownProgramPrefix.endswith("benchmarkEncoder.py")
ownProgramPrefix = ownProgramPrefix[0:len(ownProgramPrefix)-19]
 
allLines = [[int(b) for b in a.strip().split(" ")] for a in allLines]
nofObjects = max([max(a) for a in allLines])


# ==========================================
# Greedy problem solving
# ==========================================
lines = set([frozenset([a for a in b]) for b in allLines if len(b)>1])
selectedGreedy = set([a[0] for a in allLines if len(a)==1])
nofSingleEntryLines = len(selectedGreedy)
while len(lines)>0:
    # Find object with the most lines
    # 1. Count
    counter = {}
    for a in lines:
        for b in a:
            if b in counter:
                counter[b] += 1
            else:
                counter[b] = 1
    # 2. which one is largest
    maxOne = None
    maxCount = 0
    for (a,b) in counter.iteritems():
        if b>maxCount:
            maxOne = a
            maxCount = b
    # 3. Select
    selectedGreedy.add(maxOne)
    lines = set([a for a in lines if not maxOne in a])

# Find min solution
lowerBound = 1
while lowerBound*math.log(lowerBound)/math.log(2) < len(selectedGreedy)-nofSingleEntryLines:
    lowerBound += 1
lowerBound -= 1
lowerBound += nofSingleEntryLines
upperBound = len(selectedGreedy)

print("Size best solution is between "+str(lowerBound)+" and "+str(len(selectedGreedy)))


# ==========================================
# Write SMT benchmarks
# ==========================================
for actualSize in range(lowerBound,upperBound+1):
    with open(prefix+"_"+str(actualSize)+".smt2","w") as outFile:
        for i in range(1,nofObjects+1):
            outFile.write("(declare-const obj"+str(i)+" Bool)\n")
            
        for a in allLines:
            outFile.write("( assert (or "+" ".join(["obj"+str(b) for b in a])+") )\n")

        # Optimization factor
        outFile.write("(assert (>= "+str(actualSize)+" (+" + " ".join(["(ite obj"+str(b)+" 1 0)" for b in range(1,nofObjects+1)])+") ) )\n")
        outFile.write("(check-sat)\n")
        outFile.write("(get-model)\n")
        
# ==========================================
# Write SAT benchmarks
# ==========================================
for actualSize in range(lowerBound,upperBound+1):

    for (encodingName,extension) in [("BDD",".cnfBDD"),("CARDNET",".cnfCARDNET")]:
        with open(prefix+"_"+str(actualSize)+extension,"w") as outFile:
        
            p = subprocess.Popen(ownProgramPrefix+"cardinalityConstraintSATEncoder/encoder "+encodingName+" "+str(nofObjects)+" "+str(actualSize), shell=True, bufsize=1048576, stdout=subprocess.PIPE, close_fds=True)
            clauses = []
            maxVarNum = 0
            for a in p.stdout.readlines():
                line = a.strip()
                maxVarNum = max([maxVarNum]+[int(b) for b in line.split(" ")])
                clauses.append(line)
            assert p.wait()==0
        
            outFile.write("p cnf "+str(maxVarNum)+" "+str(len(clauses)+len(allLines))+"\n")
            for a in clauses:
                outFile.write(a+"\n")
            for a in allLines:
                outFile.write(" ".join([str(b) for b in a]+["0"])+"\n")


# ==========================================
# Write MAXSAT Benchmark
# ==========================================
with open(prefix+".maxsat","w") as outFile:
    outFile.write("p wcnf "+str(nofObjects)+" "+str(len(allLines)+nofObjects)+" 15\n")
    for i in xrange(1,nofObjects+1):
        outFile.write("1 -"+str(i)+" 0\n")
    for a in allLines:
        outFile.write("15 "+" ".join([str(b) for b in a]+["0"])+"\n")


