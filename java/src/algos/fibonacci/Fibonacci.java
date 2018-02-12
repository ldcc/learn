package org.ldccc.algos.fibonacci;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

public class Fibonacci {
    private static Map<Integer, BigInteger> map = new HashMap<>();

    public static BigInteger fib(BigInteger n) {

        return n.intValue() >= 0 ? fibs(n.intValue()) : fibs(Math.abs(n.intValue())).multiply(BigInteger.valueOf(-1));
//        return n.intValue() >= 0 ? posFibing(n.intValue(), BigInteger.valueOf(1)) : negFibing(n.intValue(), BigInteger.valueOf(-1));
    }

    private static BigInteger fibs(int n) {
        if (n <= 2) return BigInteger.ONE;
        BigInteger a = BigInteger.ONE;
        BigInteger b = BigInteger.ONE;
        BigInteger c = BigInteger.ZERO;
        for (int i = 2; i < n; i++) {
            c = a.add(b);
            a = b;
            b = c;
        }
        return c;
    }

    private static BigInteger posFibing(int n, BigInteger result) {
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
                    map.put(num2, posFibing(num2, result));
                    return map.get(num1).add(map.get(num2));
                }
            } else {
                map.put(num1, posFibing(num1, result));
                map.put(num2, posFibing(num2, result));
                return map.get(num1).add(map.get(num2));
            }
        }
    }

    private static BigInteger negFibing(int n, BigInteger result) {
        if (n == 0) {
            map.put(n, BigInteger.ZERO);
            return BigInteger.ZERO;
        } else if (n == -1) {
            map.put(n, result);
            return result;
        } else {
            int num1 = n + 1, num2 = n + 2;
            if (map.containsKey(num1)) {
                if (map.containsKey(num2)) {
                    return map.get(num1).add(map.get(num2));
                } else {
                    map.put(num2, negFibing(num2, result));
                    return map.get(num1).add(map.get(num2));
                }
            } else {
                map.put(num1, negFibing(num1, result));
                map.put(num2, negFibing(num2, result));
                return map.get(num1).add(map.get(num2));
            }
        }
    }

    private static BigInteger posFibingOri(int n, BigInteger result) {
        return n == 0 ? BigInteger.ZERO : n == 1 ? result : posFibingOri(n - 1, result).add(posFibingOri(n - 2, result));
    }

    private static BigInteger negFibingOri(int n, BigInteger result) {
        return n == 0 ? BigInteger.ZERO : n == -1 ? result : negFibingOri(n + 1, result).add(negFibingOri(n + 2, result));
    }
}
