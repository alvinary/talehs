def get_value(number, line):
    value = 0
    literals = line.split("), ")
    value_literals = [l for l in literals if l.startswith("bit(") and l[3:].startswith("(" + number + ",")]
    carry_literals = [l for l in literals if l.startswith("carry(") and l[5:].startswith("(" + number + ",")]
    carry_literals = sorted(carry_literals)
    carry_value = 0
    if carry_literals:
        print(carry_literals)
        if "one" in carry_literals[-2]:
            print(number, "has high carry")
            carry_value = 0
    value_literals = sorted(value_literals)
    max_i = 0
    for i, v in enumerate(value_literals):
        if 'one' in v:
            value += 2**(i)
    return value + carry_value

tests = [
    "bit(nat[1],ind[1],one), bit(nat[2],ind[1],one), bit(nat[1],ind[2],one), bit(nat[2],ind[2],zero), bit(nat[1],ind[3],zero), bit(nat[2],ind[3],zero), bit(sum[nat[1],nat[1]],ind[1],zero), bit(sum[nat[1],nat[2]],ind[1],zero), bit(sum[nat[2],nat[1]],ind[1],zero), bit(sum[nat[2],nat[2]],ind[1],zero), bit(sum[nat[1],nat[1]],ind[2],one), bit(sum[nat[1],nat[2]],ind[2],zero), bit(sum[nat[2],nat[1]],ind[2],zero), bit(sum[nat[2],nat[2]],ind[2],one), bit(sum[nat[1],nat[1]],ind[3],one), bit(sum[nat[1],nat[2]],ind[3],one), bit(sum[nat[2],nat[1]],ind[3],one), bit(sum[nat[2],nat[2]],ind[3],zero), carry(sum[nat[1],nat[1]],ind[1],zero), carry(sum[nat[1],nat[2]],ind[1],zero), carry(sum[nat[2],nat[1]],ind[1],zero), carry(sum[nat[2],nat[2]],ind[1],zero), carry(sum[nat[1],nat[1]],ind[2],one), carry(sum[nat[1],nat[2]],ind[2],one), carry(sum[nat[2],nat[1]],ind[2],one), carry(sum[nat[2],nat[2]],ind[2],one), carry(sum[nat[1],nat[1]],ind[3],one), carry(sum[nat[1],nat[2]],ind[3],one), carry(sum[nat[2],nat[1]],ind[3],one), carry(sum[nat[2],nat[2]],ind[3],zero), carry(sum[nat[1],nat[1]],ind[4],zero), carry(sum[nat[1],nat[2]],ind[4],zero), carry(sum[nat[2],nat[1]],ind[4],zero), carry(sum[nat[2],nat[2]],ind[4],zero), carry(sum[nat[1],nat[1]],ind[4],one), carry(sum[nat[1],nat[2]],ind[4],one), carry(sum[nat[2],nat[1]],ind[4],one), carry(sum[nat[2],nat[2]],ind[4],one)",
    "bit(nat[1],ind[1],one), bit(nat[2],ind[1],one), bit(nat[1],ind[2],one), bit(nat[2],ind[2],zero), bit(nat[1],ind[3],zero), bit(nat[2],ind[3],zero), bit(sum[nat[1],nat[1]],ind[1],zero), bit(sum[nat[1],nat[2]],ind[1],zero), bit(sum[nat[2],nat[1]],ind[1],zero), bit(sum[nat[2],nat[2]],ind[1],zero), bit(sum[nat[1],nat[1]],ind[2],one), bit(sum[nat[1],nat[2]],ind[2],zero), bit(sum[nat[2],nat[1]],ind[2],zero), bit(sum[nat[2],nat[2]],ind[2],one), bit(sum[nat[1],nat[1]],ind[3],one), bit(sum[nat[1],nat[2]],ind[3],one), bit(sum[nat[2],nat[1]],ind[3],one), bit(sum[nat[2],nat[2]],ind[3],zero), carry(sum[nat[1],nat[1]],ind[1],zero), carry(sum[nat[1],nat[2]],ind[1],zero), carry(sum[nat[2],nat[1]],ind[1],zero), carry(sum[nat[2],nat[2]],ind[1],zero), carry(sum[nat[1],nat[1]],ind[2],one), carry(sum[nat[1],nat[2]],ind[2],one), carry(sum[nat[2],nat[1]],ind[2],one), carry(sum[nat[2],nat[2]],ind[2],one), carry(sum[nat[1],nat[1]],ind[3],one), carry(sum[nat[1],nat[2]],ind[3],one), carry(sum[nat[2],nat[1]],ind[3],one), carry(sum[nat[2],nat[2]],ind[3],zero), carry(sum[nat[1],nat[1]],ind[4],zero), carry(sum[nat[1],nat[2]],ind[4],zero), carry(sum[nat[2],nat[1]],ind[4],zero), carry(sum[nat[2],nat[2]],ind[4],zero), carry(sum[nat[2],nat[1]],ind[4],one)",
    "bit(nat[1],ind[1],one), bit(nat[2],ind[1],one), bit(nat[1],ind[2],one), bit(nat[2],ind[2],zero), bit(nat[1],ind[3],zero), bit(nat[2],ind[3],zero), bit(nat[1],ind[4],zero), bit(nat[2],ind[4],one), bit(sum[nat[1],nat[1]],ind[1],zero), bit(sum[nat[1],nat[2]],ind[1],zero), bit(sum[nat[2],nat[1]],ind[1],zero), bit(sum[nat[2],nat[2]],ind[1],zero), bit(sum[nat[1],nat[1]],ind[2],one), bit(sum[nat[1],nat[2]],ind[2],zero), bit(sum[nat[2],nat[1]],ind[2],zero), bit(sum[nat[2],nat[2]],ind[2],one), bit(sum[nat[1],nat[1]],ind[3],one), bit(sum[nat[1],nat[2]],ind[3],one), bit(sum[nat[2],nat[1]],ind[3],one), bit(sum[nat[2],nat[2]],ind[3],zero), bit(sum[nat[1],nat[1]],ind[4],zero), bit(sum[nat[1],nat[2]],ind[4],one), bit(sum[nat[2],nat[1]],ind[4],one), bit(sum[nat[2],nat[2]],ind[4],zero), carry(sum[nat[1],nat[1]],ind[1],zero), carry(sum[nat[1],nat[2]],ind[1],zero), carry(sum[nat[2],nat[1]],ind[1],zero), carry(sum[nat[2],nat[2]],ind[1],zero), carry(sum[nat[1],nat[1]],ind[2],one), carry(sum[nat[1],nat[2]],ind[2],one), carry(sum[nat[2],nat[1]],ind[2],one), carry(sum[nat[2],nat[2]],ind[2],one), carry(sum[nat[1],nat[1]],ind[3],one), carry(sum[nat[1],nat[2]],ind[3],one), carry(sum[nat[2],nat[1]],ind[3],one), carry(sum[nat[2],nat[2]],ind[3],zero), carry(sum[nat[1],nat[1]],ind[4],zero), carry(sum[nat[1],nat[2]],ind[4],zero), carry(sum[nat[2],nat[1]],ind[4],zero), carry(sum[nat[2],nat[2]],ind[4],zero), carry(sum[nat[1],nat[1]],ind[5],zero), carry(sum[nat[1],nat[2]],ind[5],zero), carry(sum[nat[2],nat[1]],ind[5],zero), carry(sum[nat[2],nat[2]],ind[5],zero), carry(sum[nat[1],nat[2]],ind[5],one), carry(sum[nat[2],nat[1]],ind[5],one), carry(sum[nat[2],nat[2]],ind[5],one)",
    "bit(nat[1],ind[1],one), bit(nat[2],ind[1],one), bit(nat[1],ind[2],one), bit(nat[2],ind[2],zero), bit(nat[1],ind[3],zero), bit(nat[2],ind[3],zero), bit(nat[1],ind[4],zero), bit(nat[2],ind[4],one), bit(sum[nat[1],nat[1]],ind[1],zero), bit(sum[nat[1],nat[2]],ind[1],zero), bit(sum[nat[2],nat[1]],ind[1],zero), bit(sum[nat[2],nat[2]],ind[1],zero), bit(sum[nat[1],nat[1]],ind[2],one), bit(sum[nat[1],nat[2]],ind[2],zero), bit(sum[nat[2],nat[1]],ind[2],zero), bit(sum[nat[2],nat[2]],ind[2],one), bit(sum[nat[1],nat[1]],ind[3],one), bit(sum[nat[1],nat[2]],ind[3],one), bit(sum[nat[2],nat[1]],ind[3],one), bit(sum[nat[2],nat[2]],ind[3],zero), bit(sum[nat[1],nat[1]],ind[4],zero), bit(sum[nat[1],nat[2]],ind[4],one), bit(sum[nat[2],nat[1]],ind[4],one), bit(sum[nat[2],nat[2]],ind[4],zero), carry(sum[nat[1],nat[1]],ind[1],zero), carry(sum[nat[1],nat[2]],ind[1],zero), carry(sum[nat[2],nat[1]],ind[1],zero), carry(sum[nat[2],nat[2]],ind[1],zero), carry(sum[nat[1],nat[1]],ind[2],one), carry(sum[nat[1],nat[2]],ind[2],one), carry(sum[nat[2],nat[1]],ind[2],one), carry(sum[nat[2],nat[2]],ind[2],one), carry(sum[nat[1],nat[1]],ind[3],one), carry(sum[nat[1],nat[2]],ind[3],one), carry(sum[nat[2],nat[1]],ind[3],one), carry(sum[nat[2],nat[2]],ind[3],zero), carry(sum[nat[1],nat[1]],ind[4],zero), carry(sum[nat[1],nat[2]],ind[4],zero), carry(sum[nat[2],nat[1]],ind[4],zero), carry(sum[nat[2],nat[2]],ind[4],zero), carry(sum[nat[1],nat[1]],ind[5],zero), carry(sum[nat[1],nat[2]],ind[5],zero), carry(sum[nat[2],nat[1]],ind[5],zero), carry(sum[nat[2],nat[2]],ind[5],zero), carry(sum[nat[2],nat[2]],ind[5],one)",
    "bit(nat[1],ind[1],one), bit(nat[2],ind[1],one), bit(nat[1],ind[2],one), bit(nat[2],ind[2],zero), bit(nat[1],ind[3],zero), bit(nat[2],ind[3],zero), bit(nat[1],ind[4],one), bit(nat[2],ind[4],one), bit(sum[nat[1],nat[1]],ind[1],zero), bit(sum[nat[1],nat[2]],ind[1],zero), bit(sum[nat[2],nat[1]],ind[1],zero), bit(sum[nat[2],nat[2]],ind[1],zero), bit(sum[nat[1],nat[1]],ind[2],one), bit(sum[nat[1],nat[2]],ind[2],zero), bit(sum[nat[2],nat[1]],ind[2],zero), bit(sum[nat[2],nat[2]],ind[2],one), bit(sum[nat[1],nat[1]],ind[3],one), bit(sum[nat[1],nat[2]],ind[3],one), bit(sum[nat[2],nat[1]],ind[3],one), bit(sum[nat[2],nat[2]],ind[3],zero), bit(sum[nat[1],nat[1]],ind[4],zero), bit(sum[nat[1],nat[2]],ind[4],zero), bit(sum[nat[2],nat[1]],ind[4],zero), bit(sum[nat[2],nat[2]],ind[4],zero), carry(sum[nat[1],nat[1]],ind[1],zero), carry(sum[nat[1],nat[2]],ind[1],zero), carry(sum[nat[2],nat[1]],ind[1],zero), carry(sum[nat[2],nat[2]],ind[1],zero), carry(sum[nat[1],nat[1]],ind[2],one), carry(sum[nat[1],nat[2]],ind[2],one), carry(sum[nat[2],nat[1]],ind[2],one), carry(sum[nat[2],nat[2]],ind[2],one), carry(sum[nat[1],nat[1]],ind[3],one), carry(sum[nat[1],nat[2]],ind[3],one), carry(sum[nat[2],nat[1]],ind[3],one), carry(sum[nat[2],nat[2]],ind[3],zero), carry(sum[nat[1],nat[1]],ind[4],zero), carry(sum[nat[1],nat[2]],ind[4],zero), carry(sum[nat[2],nat[1]],ind[4],zero), carry(sum[nat[2],nat[2]],ind[4],zero), carry(sum[nat[1],nat[1]],ind[5],zero), carry(sum[nat[1],nat[2]],ind[5],zero), carry(sum[nat[2],nat[1]],ind[5],zero), carry(sum[nat[1],nat[1]],ind[5],one), carry(sum[nat[1],nat[2]],ind[5],one), carry(sum[nat[2],nat[1]],ind[5],one), carry(sum[nat[2],nat[2]],ind[5],one)"
]

def test_values(test_line):
    print(get_value("nat[1]", test_line))
    print(get_value("nat[2]", test_line))
    print(get_value("sum[nat[2],nat[1]]", test_line))
    print()
    assert get_value("nat[1]", test_line) + get_value("nat[2]", test_line) == get_value("sum[nat[1],nat[2]]", test_line)
    assert get_value("sum[nat[2],nat[1]]", test_line) == get_value("sum[nat[1],nat[2]]", test_line)

for test in tests:
    test_values(test)

'''
for line in lines:
    if line:
        values = {}
        numbers = get_numbers(line)
        for number in numbers:
            values[number] = get_value(number, line)
        for number in numbers:
            if is_sum(number):
                left, right, addition = read_sum(number, line)
                assert values[left] + values[right] = values[addition]
'''