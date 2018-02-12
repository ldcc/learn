package org.ldccc.algos.sis;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class SquareIntoSquare {
    /**
     * Given a positive integral number n, return a strictly increasing sequence (list/array/string depending on the language) of numbers,
     * so that the increasingSum of the squares is equal to n².
     * If there are multiple solutions (and there will be), return the result with the largest possible values.
     */

    public String demo1(String str) {
        char[] s = str.toCharArray();
        int len = s.length;
        for (int i = 1; i < 1 << len; i++) {
            StringBuilder sb = new StringBuilder();
            for (int j = 0; j < len; j++) {
                if ((i & (1 << j)) != 0) {
                    sb.append(s[j]);
                }
            }
            System.out.print(sb + " ");
        }
        return "";
    }

    //    取得最大正方形面积后，从左往右升序迭代
    public String demo2(long n) {
        int n1 = (int) n;
        int[] increasing = new int[n1];
        for (int i = 0; i < increasing.length; i++) increasing[i] = i * i;
        int square = n1 * n1;
        for (int i = increasing.length - 1; i >= 0; i--) {
            int left = square - increasing[i];
            for (int j = 1; j < 1 << increasing.length; j++) {
                int sum = 0;
                StringBuilder str = new StringBuilder();
                for (int k = 1; k < increasing.length && sum < left && k < i; k++) {
                    if ((j & (1 << k)) != 0) {
                        str.append(k).append(" ");
                        sum += increasing[k];
                        if (sum == left) return str.toString() + i;
                        //  if (sum == left) return trimList(str + i);
                    }
                }
            }
        }
        return "";
    }

    //    取得最大正方形面积后，从右往左降序迭代
    public String demo3(long n) {
        int n1 = (int) n;
        int[] increasing = new int[n1];
        for (int i = 0; i < increasing.length; i++) increasing[i] = i * i;
        int square = n1 * n1;
        for (int i = increasing.length - 1; i >= 0; i--) {
            int left = square - increasing[i];
            for (int j = 1 << increasing.length - 1; j > 0; j++) {
                int sum = 0;
                StringBuilder str = new StringBuilder();
                for (int k = 1; k < increasing.length && sum < left && k < i; k++) {
                    if ((j & (1 << k)) != 0) {
                        str.append(k).append(" ");
                        sum += increasing[k];
                        if (sum == left) return trimList(str.toString() + i);
                    }
                }
            }
        }
        return "";
    }

    private String trimList(String strList) {
        List<Integer> list = Stream.of(strList.split(" "))
                .map(Integer::parseInt)
                .collect(Collectors.toList());
        for (int i = 0; list.size() > 4 && i < list.size() - 2; i++) {
            int a = list.get(i), b = list.get(i + 1);
            double c = Math.sqrt((a * a) + (b * b));
            if (c % 1 == 0) {
                list.remove(i);
                list.remove(i);
                list.add(i, (int) c);
            }
        }
        return list.stream()
                .map(String::valueOf)
                .reduce((s1, s2) -> s1 + " " + s2)
                .orElse("");
    }

    public String decomposeDemo(long n) {
        long[] increasing = new long[Math.toIntExact(n)];
        for (int i = 0; i < increasing.length; i++) increasing[i] = i * i;
        long square = n * n;
        for (int i = increasing.length - 1; i > 0; i--) {
            long left = square - increasing[i];
            System.out.println("left" + left + ":");
            for (int j = 0; j < increasing.length; j++) {
                long increasingSum = increasingSum(increasing, j);
                System.out.println(j + ":" + increasingSum);
                if (increasingSum > left) {
                    break;
                }
                for (int k = j + 1; k < increasing.length; k++) {
                    long result = increasingSum + increasing[k];
                    if (result == left) {
                        System.out.println(j + "." + k + ":" + result);
                        return String.format("%d %d %d", j, k, i);
                    } else if (result > left) {
                        break;
                    }
                    System.out.println(j + "." + k + ":" + result);
                }
                System.out.println("-------------------");
            }
            System.out.println("-------------------------------------------------------------------");
        }
        return "";
    }

    private static long increasingSum(long[] increasing, long n) {
        if (n == 0) return increasing[Math.toIntExact(n)];
        return increasing[Math.toIntExact(n)] + increasingSum(increasing, n - 1);
    }

}
