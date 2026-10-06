import sys
from pysat.solvers import Solver
from pysat.formula import CNF

index = {}

def var(part):
    return index.setdefault(part, len(index) + 1)

def reverse_index(index_):
    reverse_index = {}
    for k in index_:
        reverse_index[index_[k]] = k
    return reverse_index

def map_line(line):
    line = line.replace(" ", "")
    if "=>" in line:
        lhs, rhs = line.split("=>")
        neg = [-var(p) for p in lhs.replace("),", ");").split(";")]
        if "False" in rhs:
            yield neg
        else:
            for p in rhs.replace("),", ");").split(";"):
                yield neg + [var(p)]
    elif ")v" in line:
        yield [var(p) for p in line.replace(")v", ");").split(";")]
    else:
        for p in line.split("v"):
            yield [var(p)]

s = Solver(name='glucose3')

def show(index_, literal):
    return index_[abs(literal)]

n = 15

for line in sys.stdin:
    line = line.rstrip("\n")
    if not line:
        break
    #print(line, list(map_line(line)))
    for clause in map_line(line):
        s.add_clause(clause)
print()
print('Models')
print()
reverse = reverse_index(index)
for i, model in enumerate(s.enum_models()):
    if i >= n:
        break
    print(", ".join([show(reverse, a) for a in model if a > 0 and "-" not in show(reverse, a)]))
    print()

s.delete()