import sys
import os
import re


def process_traces(log_file, regexes, preffix):
    print(log_file, regexes)

    remaining = open("%s-remaining-log.txt"%preffix, 'w')

    files = {}

    for r in regexes:
        files[r] = open("%s-%s-log.txt"%(preffix, r.replace("|", "-")), 'w')

    for l in open(log_file, 'r').readlines():
        found = False
        for r in regexes:
            if(re.search(r, l)):
                found = True
                files[r].write(l)
                break
        if found:
            continue
        else:
            remaining.write("%s"%l)
    remaining.close()

    for k, v in files.items():
        v.close()

def execute_wasm(file):
    pass


if __name__ == '__main__':



    log_file = sys.argv[1]
    preffix = sys.argv[2]
    regexes = sys.argv[3:]

    process_traces(log_file, regexes, preffix)