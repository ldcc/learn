package factorials;

import java.math.BigInteger;

public class LargeFactorials {
    public static String Factorial(int n) {
        return f(n, BigInteger.ONE).toString();
    }

    private static BigInteger f0(int n, BigInteger result) {
        return n <= 1 ? result : f(n - 1, result.multiply(BigInteger.valueOf(n)));
    }

    private static BigInteger f(int n, BigInteger result) {
        while (n > 0) {
            result = result.multiply(BigInteger.valueOf(n));
        }
        return result;
    }
}
