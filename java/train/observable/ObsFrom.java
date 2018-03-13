package com.ldcstorm.training.observable;

import rx.Observable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by ldcsb on 10/27/2016.
 */
public class ObsFrom {

    public static void test() {
//        Observable.from(getKudosu1())
//                .flatMap(list -> Observable.from(list))
//                .subscribe(System.out::println);
//        getKudosu2().forEach(infoList -> {
//            infoList.add("---------------------------------");
//            infoList.forEach(System.out::println);
//        });
//        insertTest();
        Observable.from(getKudosu1())
                .subscribe(System.out::println);
        getKudosu2()
                .forEach(System.out::println);
    }

    /**
     * 没加 reduce() 之前的结构是 {{a, b, c ...}, {d, e, f ...}, {g, h, i ...} ...}
     * 加了 reduce() 的结构变成了 {a, b, c ..., d, e, f ..., g, h, i ...}
     * 此时的 reduce() 与 flatMap() 的功能是类似的
     * 可以来回注释 flatMap() 进行检查
     */
    private static List<List<String>> getKudosu1() {
        List<List<String>> lists = new ArrayList<>();
        Observable.from(getNumberOfInfoList())
                .map(integer -> {
                    List<String> list = new ArrayList<>();
                    getTable().forEach(list1 -> list.add(list1.get(integer)));
//                    list.add("---------------------------------");
                    return list;
                })
//                .reduce((list1, list2) -> {
//                    list2.forEach(s -> list1.add(s));
//                    return list1;
//                })
                .subscribe(lists::add);
        return lists;
    }

    /**
     * 使用 Stream 后变得更加简洁了
     */
    private static List<List<String>> getKudosu2() {
        return getNumberOfInfoList()
                .stream().map(integer -> getTable()
                        .stream().map(list -> list.get(integer))
                        .collect(Collectors.toList()))
                .collect(Collectors.toList());
    }

    private static List<List<String>> getKudosu3() {
        List<List<String>> lists = new ArrayList<>();
        List<List<String>> kudosu = getTable();
        for (int i = 0; i < getTable().get(0).size(); i++) {
            List<String> infoList = new ArrayList<>();
            for (List<String> item : kudosu) {
                infoList.add(item.get(i));
            }
            lists.add(infoList);
        }
        return lists;
    }

    private static void insertTest() {
        List<List<String>> lists = getKudosu3();
        lists.forEach(System.out::println);
        List<String> hw = new ArrayList<>();
        hw.add("Hollow Wings");
        hw.add("518");
        hw.add("29");
        hw.add("396");
        lists.add(hw);
        System.out.println("\n");
        lists.forEach(System.out::println);
    }

    private static List<List<String>> getTable() {
        return Arrays.asList(
                Arrays.asList("Andrea", "Leorda", "Garven", "Bakari", "wmfchris", "Sonnyc", "James2250", "CDFA", "Mafiamaster", "Breeze"),
                Arrays.asList("2303", "2124", "1931", "1862", "1777", "1319", "1236", "1136", "1122", "1021"),
                Arrays.asList("35", "1665", "1140", "21", "1139", "115", "1135", "16", "48", "382"),
                Arrays.asList("2268", "459", "791", "1841", "638", "1204", "101", "1120", "1074", "639"));
    }

    private static List<Integer> getNumberOfInfoList() {
        List<Integer> list = new ArrayList<>();
        for (int i = 0; i < getTable().get(0).size(); i++) {
            list.add(i);
        }
        return list;
    }
}