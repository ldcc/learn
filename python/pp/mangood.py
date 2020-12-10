import re
import pandas
from collections import defaultdict as de


def dell(d, na):
    for i in na:
        del d[i]
    return d


def gps_de(d, n):
    name = []
    for k, v in d.items():
        if v < n:
            name.append(k)
    return dell(d, name)


def gps_cr(d):
    name = []
    for k, v in d.items():
        name.append(k)
    name_new = [i + j[-1] for i in name for j in name if i[1:] == j[:-1]]
    # gsp理论，
    return name_new


def gps_crdi(name, data):
    r = de(int)
    for i in name:
        b = ''
        for j in range(len(i) - 1):
            b += i[j] + '.*'
        b += i[-1]
        for i in data['产品组合'].values:
            i_new = i.replace(',', '')
            res = re.search(b, i_new)
            if not (res is None):
                r[b.replace('.*', '')] += 1
    return r


def calc(data):
    r_1_di = de(int)

    for i in data['产品组合'].values:
        for j in set(i.split(',')):
            r_1_di[j] += 1  # 统计各个词的频次
    di = gps_de(r_1_di, 2)  # 限制最小频数
    print('L1:')
    print(di)
    name = []
    for k, v in di.items():
        name.append(k)  # 取出需要的词
    name_new = [i + j for i in name for j in name]  # 将取出来的词自由组合
    r_di = gps_crdi(name_new, data)  # 计算各个自由组合出现的频次
    di_3 = gps_de(r_di, 2)  # 将取出来的组合再次用频次限制
    nuu = 1
    while len(di_3) != 0:
        print('L' + str(nuu + 1) + ':')
        print(di_3)
        name_n = gps_cr(di_3)
        r_di = gps_crdi(name_n, data)
        di_3 = gps_de(r_di, 2)
        nuu += 1
    return di_3


def groupby(rows):
    orders = {}
    for i, row in rows:
        guest = str(row['客户'])
        component = str(row['产品组合'])
        goods = orders.get(guest)
        if goods is None:
            goods = [component]
        else:
            goods = goods.append(component)
        print(component, goods, goods is None)
        orders[guest] = goods
    return orders


dataset = pandas.read_excel(r'dataset220201209.xlsx', sheet_name='业绩总体')

if __name__ == '__main__':
    orders = groupby(dataset.iterrows())
    dataset = pandas.DataFrame([orders]).T.reset_index().rename(columns={'index': '客户', 0: '产品组合'})
    print(dataset)
    # di_3_set = []
    # for _, row in dataset.iterrows():
    #     print(row['产品组合'])
    #     di_3_set = di_3_set.append(calc(row))
    # print(di_3_set)
