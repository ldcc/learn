def P(m, n, f):
    if n == 0:
        return 0
    else:
        return m ** (n - 1) * (1 + (m - 1) * f(m, n - 1, f)) / m ** n


def Q(m, n, f, diff=0):
    if n == 0:
        return 0
    else:
        nt = n - diff
        return m ** (n - 1) * (nt + (m - nt) * f(m, n - 1, f, diff)) / m ** n


def yjprob(m, n, f, diff):
    if n == 0:
        return 0
    if n == 100:
        return 1
    elif n <= diff:
        return P(m, n, P)
    else:
        return Q(m, n, yjprob, diff)


if __name__ == '__main__':
    # diff: 第 50 次开始概率增加
    # m: 单抽 6* 的概率为 2%，理解为一个正 50 面体的骰子时即 1/50
    # n: 抽的总次数
    diff, m, n = 50, 50, 60

    prob = yjprob(m, n, None, diff)
    print(prob)
