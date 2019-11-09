import subprocess
import itertools
import time
import sys
import os

def run_test(n, learnerFile, gladeFile):
    # where = str(n)
    # time.sleep(5)
    server = subprocess.Popen(["/usr/local/bin/stack", "run", "server", str(n)], 
        cwd="/Users/hb/latlib/Learner2")
    time.sleep(5)

    learner = subprocess.Popen(["/usr/local/bin/stack", "run", "mainDriver"], 
        stdout=learnerFile,
        cwd="/Users/hb/latlib/Learner2")
    learner.wait()

    glade = subprocess.Popen(["/usr/bin/java", "-classpath", "test:glade.jar", "main.Test"], stdout=gladeFile, cwd="/Users/hb/latlib/glade-full")
    glade.wait()

    server.kill()


learner = f"learner.txt"
glade = f"glade.txt"
try:
    os.remove(learner)
except OSError:
    pass
try:
    os.remove(glade)
except OSError:
    pass

learnerFile = open(learner, "a")
gladeFile = open(glade, "a")
for n in range(1, 12):
    # for i in range(1, 10):
    run_test(n, learnerFile, gladeFile)
learnerFile.close()
gladeFile.close()
