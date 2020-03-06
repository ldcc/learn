# 一个正 m 面体骰子，该骰子骰 n 次时某一数字出现过的概率
def P(m, n):
    return 0 if n == 0 else m ** (n - 1) * (1 + (m - 1) * P(m, n - 1)) / m ** n


def Pc(m, n):
    return 0 if n == 0 else m ** (n - 1) * (n + (m - n) * Pc(m, n - 1)) / m ** n


def Pf(m, n, limit):
    pass


# n 为抽卡次数
def yjSteinLottery(n):
    # 单抽 6* 的官方公布概率为 2%，即可以理解为一个正 50 面体的骰子
    # 而你要抽到 6* 则表示你需要吧骰子骰到带有 6* 的一面

    # 第 50 次开始概率增加
    m = 50


if __name__ == '__main__':
    print(Pc(50, 10))
