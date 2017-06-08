import glob

# Benchmark/Tool combinations
toolBenchmarkPairs = [(glob.glob("*.cnfBDD"),["picosat","minisat","lingeling","mapleCOMSPS"]),(glob.glob("*.cnfCARDNET"),["picosat","minisat","lingeling","mapleCOMSPS"]),(glob.glob("*.smt2"),["z3"])]

TIME_LIMIT_IN_SECONDS = 60
MEMORY_LIMIT_IN_KB = 2048000
# TIME_STEPS_DRAW = [0.1,1,10,100,1000,1800]
TIME_STEPS_DRAW = [0.1,1,10,55]
IMAGE_HEIGHT = 8.2
XSCALE = 0.5

DRAW_STYLES = ["color=blue", "color=green", "color=red", "color=yellow!80!black","color=blue!50!red,dotted,thick","color=black,dashed", "color=red!50!black,thick", "color=green,dashed", "color=blue,thick,dashed" ]
