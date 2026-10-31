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

def split_top(s, sep=","):
    parts, depth, cur = [], 0, []
    for ch in s:
        if ch in "([":
            depth += 1
        elif ch in ")]":
            depth -= 1
        if ch == sep and depth == 0:
            parts.append("".join(cur))
            cur = []
        else:
            cur.append(ch)
    parts.append("".join(cur))
    return parts

def map_line(line):
    line = line.replace('"', "").replace(" ", "")
    if "=>" in line:
        lhs, rhs = line.split("=>")
        neg = [-var(p) for p in split_top(lhs)]
        if rhs == "False":
            yield neg
        else:
            for p in split_top(rhs):
                yield neg + [var(p)]
    elif "||" in line:
        yield [var(p) for p in line.split("||")]
    else:
        for p in split_top(line):
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
    return "-" in literal or "=" in literal or "bits[" in literal

print()
print('Models')
print()
reverse = reverse_index(index)
c = 0
for i, model in enumerate(s.enum_models()):
    if i >= n:
        break
    c += 1
    print(", ".join([show(reverse, a) for a in model if a > 0 and not must_hide(show(reverse, a))]))
    print()
print("Shown ", c, " models")

s.delete()