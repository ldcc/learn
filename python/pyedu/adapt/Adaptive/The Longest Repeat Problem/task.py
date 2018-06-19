#  this is a the longest repeat problem task. You can use this editor as a playground

f = open("dataset.txt", "r")
data = f.readline()
f.close()

longest = ""

while data:
    halfLen = len(data) // 2

    if len(longest) > halfLen:
        break

    for i in range(halfLen, len(longest), -1):
        expect = data[:i + 1]
        tail = data[i + 1:]

        if expect in tail:
            longest = expect
            break

    data = data[1:]


print(longest)
