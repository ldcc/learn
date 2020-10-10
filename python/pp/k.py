# from pulp import *
from sympy import *

import pandas as pd

R_all = pd.read_table('7-12.txt', header=None)


def getresult():
    f_c = [80, 81, 81, 82, 84, 83]  # 单位粉丝成本
    f_v = [56, 57, 59, 56, 65, 65]  # 单位粉丝价值
    t = [0.11, 0.11, 0.12, 0.11, 0.14, 0.14]  # 粉丝转化率
    p = [1405, 1400, 1428, 1562, 1410, 1410]  # 复购客单价，以上都为7-12月
    xt = [6516, 5716, 6461, 4426, 6245, 6747, 5811, 7112, 7002, 3343, 1668, 13421, 12861, 14169, 14281]  # 19.4-20.6 约单数
    xtlen = len(xt)
    # 复购每个月的留存
    rs = [R_all.iloc[:, i] for i in range(6)]
    # 每个月增值部业绩和16个月以前的营销留存
    y_zengzhi = [346975, 362112, 353667, 384863, 404928, 375722]
    y_yiliu = [1000000, 1000000, 1000000, 1000000, 1000000, 1000000]
    m_roi = 2.2
    min_roi = 2.5
    m_c = 50000000
    xs = [Symbol(x) for x in ["x0", "x1", "x2", "x3", "x4", "x5"]]
    xslen = len(xs)

    def countp(i, rlst, xt_i):
        if i < xtlen:
            return xt[i] * rlst[xt_i] + countp(i + 1, rlst, xt_i + 1)
        else:
            if xt_i > xtlen:
                return 0
            else:
                k = i - xtlen
                return xs[k] * t[k] * rlst[xt_i] + countp(i + 1, rlst, xt_i + 1)

    yeji = 1 / sum([(xs[i] * f_v[i] + y_zengzhi[i] + y_yiliu[i] + p[i] * countp(i, rs[i], 0)) for i in range(xslen)])
    print(yeji)
    z = sum([xs[i] * f_c[i] for i in range(xslen)]) * yeji
    print(z)

    # prob += sum([(xs[i] * f_v[i] + y_zengzhi[i] + y_yiliu[i] + p[i] * countp(i, rs[i], 0)) / xs[i] * f_c[i] - m_roi for i in range(xslen)])
    # prob += m_c - sum([xs[i] * f_c[i] for i in range(xslen)])
    # prob += sum([xs[i] * f_v[i] + y_zengzhi[i] + y_yiliu[i] + p[i] * countp(i, rs[i], 0) for i in range(xslen)])
    # print(prob)

    # 求解
    # ret = solve(prob, xs)
    # print(ret)


if __name__ == '__main__':
    getresult()
