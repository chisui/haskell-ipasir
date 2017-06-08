#!/usr/bin/env python3
# Generate SAT instances for factoring integers
import sys, glob, os, math
from config import TIME_LIMIT_IN_SECONDS, TIME_STEPS_DRAW, IMAGE_HEIGHT, XSCALE, DRAW_STYLES

def timeLogConverter(thetime):
    if (thetime)<0.05:
        return 0.0
    return IMAGE_HEIGHT/(math.log(TIME_LIMIT_IN_SECONDS)-math.log(0.05))*(math.log(thetime)-math.log(0.05))  

allBenchmarkFiles = glob.glob("*.txt")
for a in allBenchmarkFiles:
    if "_" in a:
        print("Error: Benchmark file names must noch contain underscore.",file=sys.stderr)
        sys.exit(1)

allResultFiles = glob.glob(".res/*")

        
print("\\documentclass[halfparskip,DIV18]{scrartcl}")
print("\\usepackage{tikz}")
print("\\title{Benchmark resuls}")
print("\\begin{document}\n\\maketitle")
for baseBenchmark in allBenchmarkFiles:
    print("\\section{Benchmark: "+baseBenchmark+"}")
    baseBenchmark = baseBenchmark[0:len(baseBenchmark)-4]
    
    # Obtain Min/Max number of objects
    # Also obtain the combinations
    minNumObjects = 12345678
    maxNumObjects = -1
    variants = set([])
    for a in allResultFiles:
        assert a.startswith(".res/")
        a = a[5:]
        if a.startswith(baseBenchmark):
            # This one
            parts = a.split("_")
            suffix = parts[1][parts[1].index(".")+1:]
            numObjects = int(parts[1][0:parts[1].index(".")])
            if numObjects<minNumObjects:
                minNumObjects = numObjects
            if numObjects>maxNumObjects:
                maxNumObjects = numObjects
            if not suffix.endswith(".stderr"):
                variants.add(suffix)
                
    # Read data
    
    if maxNumObjects>0:
        drawData = {}
        isSAT = {}
        for variant in variants:
            for i in range(minNumObjects,maxNumObjects+1):
                thisFilename = ".res/"+baseBenchmark+"_"+str(i)+"."+variant+".stderr"
                thisFilenameResult = ".res/"+baseBenchmark+"_"+str(i)+"."+variant
                thisTime = None
                try:
                    with open(thisFilename,"r") as dataFile:
                        for line in dataFile:
                            line = line.strip().split(" ")
                            if line[0] == "FINISHED":
                                assert line[1]=="CPU"
                                thisTime = float(line[2])
                                
                                
                   # Ok, then also check if SAT
                    with open(thisFilenameResult,"r") as dataFile:
                        for line in dataFile:
                            if line.strip()=="s SATISFIABLE":
                                assert not i in isSAT or isSAT[i]==True
                                isSAT[i] = True              
                            elif line.strip()=="s UNSATISFIABLE":
                                assert not i in isSAT or isSAT[i]==False
                                isSAT[i] = False
                except OSError:
                    pass # Ok, so no data.
                drawData[(variant,i)] = thisTime
                
        # Draw data
        image_width = maxNumObjects-minNumObjects
        print("\\begin{tikzpicture}[xscale="+str(XSCALE)+"]")
        print("\\draw[->] (0,0) -- (0,"+str(IMAGE_HEIGHT)+");")
        print("\\draw[->] (0,0) -- ("+str(image_width)+",0) -- ("+str(image_width)+","+str(IMAGE_HEIGHT)+");")
        print("\\node[draw,anchor=north west] at (0,-1) { \\begin{tabular}{l}")
        for i,a in enumerate(variants):
            if i!=0:
                print("\\\\")
            print("\\begin{tikzpicture} \draw["+DRAW_STYLES[i]+"] (0,0) -- (2,0); \\end{tikzpicture} "+a)
        print("\\end{tabular}};")

        # X Axis Description
        for i in range(minNumObjects,maxNumObjects+1):
            data = str(i)
            if i in isSAT:
                if isSAT[i]:
                    data = "\\textbf{"+data+"}"
                elif not isSAT[i]:
                    data = "\\textit{"+data+"}"
            print("\\draw ("+str(i-minNumObjects)+",0.1) -- +(0,-0.2) node[below] {"+data+"};")
            
        # Y Axis Description
        for i in TIME_STEPS_DRAW:
            y = timeLogConverter(i)
            data = str(i)
            print("\\draw (0.1,"+str(y)+") -- +(-0.2,0) node[left] {"+data+"};")  
        
        # Draw data
        for j,variant in enumerate(variants):
            lastPoints = []
            for i in range(minNumObjects,maxNumObjects+1):
                if drawData[(variant,i)]==None:
                    # Draw if not yet done
                    if lastPoints!=[]:
                        if len(lastPoints)==1:
                            # Single point
                            print ("\\draw["+DRAW_STYLES[j]+"] (",i-minNumObjects-1,",",str(timeLogConverter(lastPoints[0]))+") circle (0.1cm);")
                        else:
                            print ("\\draw[thick,"+DRAW_STYLES[j]+"] "+" -- ".join(["("+str(i-minNumObjects-len(lastPoints)+k)+","+str(timeLogConverter(lastPoints[k]))+")" for k in range(0,len(lastPoints))])+";")
                        lastPoints = []
                else:
                    lastPoints.append(drawData[(variant,i)])
            # Finalize...
            if len(lastPoints)==1:
                # Single point
                print ("\\draw["+DRAW_STYLES[j]+"] (",maxNumObjects-minNumObjects,",",str(timeLogConverter(lastPoints[0]))+") circle (0.1cm);")
            elif len(lastPoints)>1:
                print ("\\draw[thick,"+DRAW_STYLES[j]+"] "+" -- ".join(["("+str(maxNumObjects-minNumObjects-len(lastPoints)+k+1)+","+str(timeLogConverter(lastPoints[k]))+")" for k in range(0,len(lastPoints))])+";")
        
        print("\\end{tikzpicture}")
        
    else:
        print("Not enough benchmarks have been computed.")
    
    

print("\\end{document}")
