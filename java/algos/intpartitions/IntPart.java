package org.ldccc.algos.intpartitions;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class IntPart {
    public static String part(long n) {
        List<Long> list = new ArrayList<>();
        list.add(n);
        parseDemo(list);
//        parse(list, n);
        System.out.println(lists);
        return "";
    }

    private static final List<List<Long>> lists = new ArrayList<>();

    private static void parseDemo(List<Long> list) {
        lists.add(list);
        List<Long> list1 = list.stream().collect(Collectors.toList());
        if (list.get(0) > 1) {
            list1.add(list1.set(0, list1.get(0) - 1)-list1.get(0));
            parseDemo(list1);
        }
    }

    private static void parse(List<Long> list, long n) {
        lists.add(list);
        List<Long> list1 = list.stream().collect(Collectors.toList());
        if (list.get(0) > 1) {
            System.out.println(list.get(0));
            list1.set(0, list1.get(0) - 1);
            for (int j = 0; j < list1.size(); j++) {
                if (j + 1 < list1.size() && list1.get(j) > list1.get(j + 1)) {
                    list1.set(j + 1, list1.get(j + 1) + 1);
                } else {
                    list1.add(1L);
                }
            }
            parse(list1, n);
        }
    }

}
