package org.ldccc.algos.bitcounting;

public class BitCounting {

    public static int countBits(int n) {
        char[] cs = Integer.toBinaryString(n).toCharArray();
        int result = 0;
        for (char c : cs) result += c == '1' ? 1 : 0;
        return result;
    }
}
