package org.ldccc.algos.ischeating;

import java.util.ArrayList;
import java.util.List;

public class Cheating {
    public static List<long[]> removNb(long n) {
        List<long[]> list = new ArrayList<>();
        if (n < 1) return list;
        long sum = sum(n);
//        System.out.println(sum);
        for (long a = 1; a <= n; a++) {
            for (long b = 1; b <= n; b++) {
                if (a * n < sum - a - b) break;
                if (a * b > sum) break;
                if (a * b == sum - a - b) list.add(new long[]{a, b});
            }
        }
        return list;
    }

    private static long sum(long n) {
        long sum = 0;
        while (n > 0) sum += n--;
        return sum;
    }

    private static long sum(long n, long sum) {
        return n < 1 ? sum : sum(n - 1, sum + n);
    }

}
