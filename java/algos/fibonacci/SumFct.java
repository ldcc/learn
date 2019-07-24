package fibonacci;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

public class SumFct {
    private static Map<Integer, BigInteger> map = new HashMap<>();

    public static BigInteger perimeter(BigInteger n) {
        return fibs(n.intValue()).multiply(BigInteger.valueOf(4));
//        map.clear();
//        setFibMap(n.intValue() + 2, BigInteger.ONE);
//        return map.values().stream()
//                .reduce(BigInteger::add)
//                .orElse(BigInteger.ZERO)
//                .multiply(BigInteger.valueOf(4));
    }

    private static BigInteger fibs(int n) {
        BigInteger a = BigInteger.ZERO;
        BigInteger b = BigInteger.ONE;
        BigInteger c = BigInteger.ZERO;
        BigInteger sum = BigInteger.ZERO;
        for (int i = 0; i <= n; i++) {
            a = b;
            b = c;
            c = a.add(b);
            sum = sum.add(c);
        }
        return sum;
    }

    private static BigInteger setFibMap(int n, BigInteger result) {
        if (n == 0) {
            map.put(n, BigInteger.ZERO);
            return BigInteger.ZERO;
        } else if (n == 1) {
            map.put(n, result);
            return result;
        } else {
            int num1 = n - 1, num2 = n - 2;
            if (map.containsKey(num1)) {
                if (map.containsKey(num2)) {
                    return map.get(num1).add(map.get(num2));
                } else {
                    map.put(num2, setFibMap(num2, result));
                    return map.get(num1).add(map.get(num2));
                }
            } else {
                map.put(num1, setFibMap(num1, result));
                map.put(num2, setFibMap(num2, result));
                return map.get(num1).add(map.get(num2));
            }
        }
    }
}
