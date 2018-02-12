package com.ldcstorm.training.java8news;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * Created by ldcsb on 10/30/2016.
 */
public class OptionalTest {

    public static void test() {
        List<String> list = Arrays.asList(foo(), foo2(), foo3(),foo4());
        list.forEach(OptionalTest::printFoo);
//        list.forEach(System.out::println);
    }

    private static void printFoo(String s) {
        System.out.println(s);
    }

    /**
     * Optional 中的数据为 null 时无法对 Optional 进行操作，
     * 否则将会捕获一个异常
     * 除非你使用了 Java 8 提供的一系列古怪的函数式编程操作，
     * 比如 map, flatMap, orElse 等等，把它们组合起来
     */
    private static Optional<String> find() {
        String s = "Hello Java8";
        return Optional.of(s);
//        return Optional.empty();
    }

    /**
     * 当 Optional 中的数据为 null 时的处理方法
     * isPresent() 用于判断其值是否为空
     * 比如为空时返回一个空字符串
     */
    private static String foo() {
        Optional<String> found = find();
        return found.isPresent() ? found.get() : "I'm null";
    }

    /**
     * ifPresent() 能够对其中数据进行进一步的处理
     * 你可以通过 Consumer.accept(T) 方法随意改变这份数据的内容
     * 比如在不为空时返回被你重新修改过的数据
     */

    private static String foo2() {
        Optional<String> found = find();
        if (found.isPresent()) {
            found.ifPresent(content -> content += content + " Ver.2");
            return found.get();
        } else {
            return "I'm null";
        }
    }

    /**
     * 当 Optional 中得数据为空时
     * orElse() 方法提供一个备选值
     * 当备选值需要经过繁琐计算时
     * orElseGet() 方法可传入一条 lambda 表达式对备选值进行计算
     */
    private static String foo3() {
//        return find().orElse("I'm null");
        return find().orElseGet(() -> "I'm null");
    }

    /**
     * foo3() 和 foo4() 在一起可以写得更加简洁
     * 作用和 foo2() 一样，不为空时改变内容，为空时输出空字符串
     */
    private static String foo4() {
        Optional<String> found = find();
        found.ifPresent(content -> content += content + " Ver.2");
        return found.orElse("I'm null");
    }
}
