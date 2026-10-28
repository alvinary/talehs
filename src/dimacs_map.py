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
    line = line.replace('"', "")
    line = line.replace(" ", "")
    if "=>" in line:
        lhs, rhs = line.split("=>")
        neg = [-var(p) for p in lhs.replace("),", ");").split(";")]
        if "False" in rhs:
            yield neg
        else:
            for p in rhs.replace("),", ");").split(";"):
                yield neg + [var(p)]
    elif ")||" in line:
        yield [var(p) for p in line.replace(")||", ");").split(";")]
    else:
        for p in line.split("||"):
            yield [var(p)]

s = Solver(name='glucose3')

def show(index_, literal):
    return index_[abs(literal)]

def present(index_, literal):
    if literal < 0:
        return "-" + show(index_, literal)
    elif literal > 0:
        return show(index_, literal)

n = int(input())

debug = False

if debug:
    for line in sys.stdin:
        line = line.rstrip("\n")
        if not line:
            break
        #print(line, list(map_line(line)))
        print("Now mapping ", line)
        clauses = []
        for clause in map_line(line):
            s.add_clause(clause)
            reverse = reverse_index(index)
            clauses.append(" v ".join([present(reverse, a) for a in clause]))
        for c in clauses:
            print("Mapped it to", c)
        print()
else:
    for line in sys.stdin:
        line = line.rstrip("\n")
        if not line:
            break
        for clause in map_line(line):
            s.add_clause(clause)

def must_hide(literal):
    return "-" in literal or "=" in literal # or "bits[" in literal

print()
print('Models')
print()
reverse = reverse_index(index)
for i, model in enumerate(s.enum_models()):
    if i >= n:
        break
    print(", ".join([show(reverse, a) for a in model if a > 0 and not must_hide(show(reverse, a))]))
    print()
print("Shown ", i + 1, " models")

s.delete()