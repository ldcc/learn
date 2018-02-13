package com.ldcstorm.training.observable;

import rx.Observable;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by ldcsb on 10/26/2016.
 */
public class ObsJust {

    /**
     * Operator 继承了 Func1，却不能作为参数传入
     * OnSubscribe 继承了 Action1，却不能作为参数传入
     */
    public static void test() {
        final int[] i = {0};
        Observable.just("Cookiezi", "Angelsim", "Rafis", "hvick225", "Vaxei", "filsdelama", "Dustice", "Sqare", "Axarious", "Recia")
                .map(s -> {
                    Map<String, Integer> map = new HashMap<>();
                    map.put(s, ++(i[0]));
                    return map;
                })
//                .reduce((map, map2) -> {
//                    map.keySet().forEach(s -> System.out.println("map:" + s));
//                    map.keySet().forEach(s -> System.out.println("map2:" + s));
//                    return new HashMap<>();
//                })
                .subscribe(map -> map.keySet().forEach(
                        key -> System.out.println(map.get(key) + ":" + key)));
    }


}
