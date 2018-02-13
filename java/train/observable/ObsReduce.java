package com.ldcstorm.training.observable;



import rx.Observable;

import java.util.Arrays;
import java.util.List;

/**
 * Created by ldcsb on 10/30/2016.
 */
public class ObsReduce {

    /**
     * 第一次在我的 Test2 里测试 reduce() 的时候，为了知道第一个参数和第二个参数都代表了什么，
     * 我把每个 map 的 key 转换成 set 加上这个 map 的标签逐个打印，
     * 然后在 reduce() 里 return 了一个 new HashMap() 免得 subscribe() 打印出其他多余的东西影响观看……
     * 于是我看到屏幕上第一个 set 只在第一次递归时打印了第一个创建的 map 的元素，第二个 set 打印了其他元素，
     * 这是由于每次 return 的值都会作为第二次递归时的第一个参数，
     * 于是空的 set 什么都没打印出来，直接就打印了第二个 map */
    public static void test() {
        List<Integer> list = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
        Observable.from(list)
                .map(integer -> integer * integer)
                .reduce((x, y) -> x + y)
                .subscribe(System.out::println);
        Observable.from(list)
                .reduce((x, y) -> x + (y * y))
                .subscribe(System.out::println);
    }
}
