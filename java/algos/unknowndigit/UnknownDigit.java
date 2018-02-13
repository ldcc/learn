package org.ldccc.algos.unknowndigit;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

public class UnknownDigit {
    public static int solveExpression(final String expression) {
        int missingDigit = -1;
        if (expression == null || expression.length() == 0) return missingDigit;

        List<Integer> numList = new ArrayList<>();
        for (char c : expression.toCharArray()) {
            if (c > 47 && c < 58 && !numList.contains((int) c - 48)) numList.add((int) c - 48);
        }

        String[] chars = expression.split("=");
        if (chars.length != 2) return missingDigit;
        String exp = chars[0];
        String sol = chars[1];

        int runes1 = 1;
        int runes2 = 1;

        if (exp.charAt(0) == '-') {
            runes1 = -1;
            exp = exp.substring(1);
        }

        int mid = exp.lastIndexOf('-');
        if (mid != -1) {
            String sy = exp.substring(mid - 1, mid + 1);
            if (sy.equals("+-") || sy.equals("--") || sy.equals("*-") || sy.equals("/-")) {
                runes2 = -1;
                String l = exp.substring(0, mid);
                String r = exp.substring(mid + 1, exp.length());
                exp = l + r;
            }
        }

        String[] nums = exp.split("\\+|-|\\*|/");
        if (nums.length != 2) return missingDigit;
        String e1 = nums[0];
        String e2 = nums[1];

        int c1 = 0, c2 = 0, solc = 0;

        for (char c : e1.toCharArray()) if (c == '?') c1++;
        for (char c : e2.toCharArray()) if (c == '?') c2++;
        for (char c : sol.toCharArray()) if (c == '?') solc++;

        boolean b = sol.matches("\\?\\?+");

        e1 = e1.replace("?", "%d");
        e2 = e2.replace("?", "%d");
        sol = sol.replace("?", "%d");

        char symbol = 0;
        for (char c : exp.substring(1).toCharArray()) {
            if (c == '+' || c == '-' || c == '*' || c == '/') {
                symbol = c;
                break;
            }
        }

        for (int i = 0; i < 10; i++) {
            if (b && i == 0 || numList.contains(i)) continue;
            double result = calc(getNumber(e1, c1, i, runes1), getNumber(e2, c2, i, runes2), symbol);
            if (result == getNumber(sol, solc, i, 1)) return i;
        }
        return missingDigit;
    }

    private static double calc(double num1, double num2, char symbol) {
        return symbol == '+' ? num1 + num2 : symbol == '-' ? num1 - num2 : symbol == '*' ? num1 * num2 : num1 / num2;
    }

    private static double getNumber(String e, int c, int missing, int runes) {
        return Double.parseDouble(String.format(e, Stream.of(new Object[c]).map(o -> missing).toArray())) * runes;
    }
}
