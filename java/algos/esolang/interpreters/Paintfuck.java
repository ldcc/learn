package esolang.interpreters;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;

public class Paintfuck {
    public static String interpreter(String code, int iterations, int width, int height) {
        code = code.replaceAll("[^nesw\\[\\]*]*", "");
        int[][] tapes = new int[height][width];
        List<Integer> pres = new ArrayList<>();
        Stack<Boolean> braces = new Stack<>();
        braces.push(true);
        int currX = 0;
        int currY = 0;
        for (int i = 0; i < code.length() && iterations > 0; i++, iterations--) {
            if (currY >= tapes.length) {
                currY = 0;
            } else if (currX >= tapes[0].length) {
                currX = 0;
            } else if (currY < 0) {
                currY = tapes.length - 1;
            } else if (currX < 0) {
                currX = tapes[0].length - 1;
            }
            if (code.charAt(i) == ']' || code.charAt(i) == '[') {
                if (code.charAt(i) == ']') {
                    braces.pop();
                    i = tapes[currY][currX] != 0 ? pres.get(braces.size() - 1) : i;
                } else {
                    braces.push(tapes[currY][currX] != 0);
                    pres.add(i - 1);
                    iterations++;
                }
            } else if (braces.peek()) {
                if (code.charAt(i) == 'e') {
                    ++currX;
                } else if (code.charAt(i) == 'w') {
                    --currX;
                } else if (code.charAt(i) == 's') {
                    ++currY;
                } else if (code.charAt(i) == 'n') {
                    --currY;
                } else if (code.charAt(i) == '*') {
                    tapes[currY][currX] ^= 1;
                }
            } else {
                iterations++;
            }
        }
        return toTapes(tapes);
    }

    private static String toTapes(int[][] tapes) {
        return Arrays.stream(tapes)
                .map(tape -> Arrays.stream(tape)
                        .mapToObj(String::valueOf)
                        .collect(Collectors.joining()))
                .collect(Collectors.joining("\r\n"));
    }
}
