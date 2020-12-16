import re
import pandas
from mpmath.calculus.extrapolation import fold_finite


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


def gps_cr(d, first):
    name = d.keys()
    if first:
        name_new = [i + j for i in name for j in name]
    else:
        name_new = [i + j[-1] for i in name for j in name if i[1:] == j[:-1]]
    return name_new


def gps_crdi(name, data):
    r = {}
    for i in name:
        b = ''
        for j in range(len(i) - 1):
            b += i[j] + '.*'
        b += i[-1]
        for o in data:
            i_new = o.replace(',', '')
            res = re.search(b, i_new)
            if res is not None:
                v = b.replace('.*', '')
                r[v] = r.setdefault(v, 0) + 1
    return r


def gps(data):
    r_di = {}
    for i in data:
        for j in set(i.split(',')):
            r_di[j] = r_di.setdefault(j, 0) + 1

    di = gps_de(r_di, 2)
    nuu = 1
    ret = {}
    while True:
        print('L' + str(nuu) + ':', di)
        name_n = gps_cr(di, nuu == 1)
        r_di = gps_crdi(name_n, data)
        di = gps_de(r_di, 2)
        if len(di) == 0:
            break
        ret.update(di)
        nuu += 1
    return ret


def groupby(rows):
    orders = {}
    for i, row in rows:
        guest = str(row['客户'])
        prods = set(str(row['产品组合']).split(','))
        goods = orders.get(guest)
        if goods is None:
            goods = [prods]
        else:
            goods.append(prods)
        orders[guest] = goods
    return orders


def calc():
    orders = groupby(dataset.iterrows())
    di_3s = {}
    print(orders)
    return di_3s
    for k, order in orders.items():
        print('--------------------------------------------')
        print(order)
        di_3 = gps(order)
        if di_3 is not None and len(di_3) != 0:
            di_3s[k] = di_3
    return di_3s


dataset = pandas.read_excel(r'dataset220201209.xlsx', sheet_name='业绩总体')
if __name__ == '__main__':
    print(dataset)
    gpsde = calc()
    print(gpsde)
