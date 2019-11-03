import subprocess
import time
import sys
import os

def run_test(n, learnerFile, gladeFile):
    # where = str(n)
    server = subprocess.Popen(["/usr/local/bin/stack", "run", "server", str(n)], cwd="/Users/hb/latlib/Learner2")
    time.sleep(5)
    learner = subprocess.Popen(["/usr/local/bin/stack", "run", "schemaGen"], 
        stdout=learnerFile,
        cwd="/Users/hb/latlib/Learner2")
    learner.wait()
    glade = subprocess.Popen(["/usr/bin/java", "-classpath", "test:glade.jar", "main.Test"], stdout=gladeFile, cwd="/Users/hb/latlib/glade-full")
    glade.wait()
    server.kill()

try:
    os.remove("learner.txt")
except OSError:
    pass
try:
    os.remove("glade.txt")
except OSError:
    pass
learnerFile = open("learner.txt", "a")
gladeFile = open("glade.txt", "a")
for i in range(1, 10):
    run_test(i, learnerFile, gladeFile)
