package com.ldcstorm.training.java8news;

/**
 * Created by ldcsb on 01/25/2017.
 */
public class MultipleInheritance {

    public static void test() {
        MusicalJukebox jukebox = new MusicalJukebox() {};
        System.out.println(jukebox.rock());

        MusicalCarriage carriage = new MusicalCarriage();
        System.out.println(carriage.rock());
    }

    private interface Jukebox {
        default String rock() {
            return "... all over the world";
        }
    }

    private interface Carriage {
        default String rock() {
            return "... from side to side";
        }
    }

    /**
     * 默认方法相当于一个普通类的已实现方法
     * 因此在多继承（interface）或多实现（class）时有时会出现方法指向不明确的现象
     * 所以需要用到增强的 super 语法明确指明默认方法
     */
    private interface MusicalJukebox extends Jukebox, Carriage {
        @Override
        default String rock() {
            return Jukebox.super.rock();
        }
    }

    private static class MusicalCarriage implements Jukebox, Carriage {
        @Override
        public String rock() {
            return Carriage.super.rock();
        }
    }
}
