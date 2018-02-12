package org.ldccc.algos.histogram;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public class Dinglemouse {
    public static String histogram(final int results[]) {
        if (results == null || results.length == 0) return "";
        List<List<String>> before = new ArrayList<>();
        int max = Arrays.stream(results).max().orElse(0);

        for (int i = 0; i < results.length; i++) {
            List<String> list = new ArrayList<>();
            List<String> line = new ArrayList<>();
            for (int j = max; j >= 0; j--) {
                histogramAdd(list, line, j >= results[i] ? j == results[i] && results[i] != 0 ? results[i] > 9 ?
                        new Object[]{results[i] / 10, results[i] % 10} :
                        new Object[]{results[i], " "} :
                        new Object[]{" ", " "} :
                        new Object[]{"#", " "});
            }
            histogramAdd(list, line, new Object[]{"-", i != results.length - 1 ? "-" : " "});
            histogramAdd(list, line, new Object[]{i + 1, " "});
            before.add(list);
            before.add(line);
        }

//        the key
        List<List<String>> after = numberList(before.get(0).size()).stream()
                .map(i -> before.stream()
                        .map(list -> list.get(i))
                        .collect(Collectors.toList()))
                .collect(Collectors.toList());
        after.forEach(list -> list.add("\n"));

        return after.stream()
                .flatMap(Collection::stream)
                .reduce((s1, s2) -> s1 + s2)
                .orElse("");

//        sec way
//        List<String> after = numberList(before)
//                .stream()
//                .map(i -> before.stream()
//                        .map(list -> list.get(i))
//                        .reduce((s1, s2) -> s1 + s2)
//                        .orElse(""))
//                .collect(Collectors.toList());
//
//        for (int i = 0; i < after.size(); i++) {
//            char[] chars = after.get(i).toCharArray();
//            int count = chars.length;
//            for (int j = chars.length - 1; j >= 0; j--) {
//                if (chars[j] == ' ') {
//                    count--;
//                } else {
//                    break;
//                }
//            }
//            after.set(i, after.get(i).substring(0, count) + (after.get(i).trim().length() != 0 ? "\n" : ""));
//        }
//
//        return after.stream()
//                .reduce((s1, s2) -> s1 + s2)
//                .orElse("");
    }

    private static List<Integer> numberList(int beforeSize) {
        List<Integer> list = new ArrayList<>();
        for (int i = 0; i < beforeSize; i++) list.add(i);
        return list;
    }

    private static void histogramAdd(List<String> list, List<String> line, Object[] args) {
        list.add(args[0].toString());
        line.add(args[1].toString());
    }
}
