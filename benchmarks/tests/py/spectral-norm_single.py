#!/usr/bin/env python3
# Single-threaded version of spectral-norm for fair comparison
# Based on the benchmarks game version but without multiprocessing

from math import sqrt
from sys import argv


def eval_A(i, j):
    ij = i + j
    return ij * (ij + 1) // 2 + i + 1


def A_sum(u, i):
    return sum(u_j / eval_A(i, j) for j, u_j in enumerate(u))


def At_sum(u, i):
    return sum(u_j / eval_A(j, i) for j, u_j in enumerate(u))


def multiply_AtAv(u):
    n = len(u)
    tmp = [A_sum(u, i) for i in range(n)]
    return [At_sum(tmp, i) for i in range(n)]


def main():
    n = int(argv[1])
    u = [1] * n

    for _ in range(10):
        v = multiply_AtAv(u)
        u = multiply_AtAv(v)

    vBv = vv = 0

    for ue, ve in zip(u, v):
        vBv += ue * ve
        vv  += ve * ve

    result = sqrt(vBv/vv)
    print("{0:.9f}".format(result))


if __name__ == '__main__':
    main()
