glade = open("glade.txt", "r")
learner = open("learner.txt", "r")
def tabedTuples(file):
    for line in file.readlines():
        strTuple = line.split("\t")
        strTuple[-1] = strTuple[-1][:-2]
        numTuple = list(map(lambda s: float(s.strip()), strTuple))
        yield numTuple

print(list(tabedTuples(glade)))
print(list(tabedTuples(learner)))