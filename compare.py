

f1 = open("trace.mem", 'r').readlines()
f2 = open("trace2.mem", 'r').readlines()

if len(f1) != len(f2):
    print("Diff mem size")
    exit(1)

print(len(f1), len(f2))

ex = 0
for i, l1 in enumerate(f1):
    l2 = f2[i]

    if l1 != l2:
        print(i, l1.replace("\n", "" ), l2.replace("\n", ""))
        ex = 1

exit(ex)