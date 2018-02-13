package com.ldcstorm.training.java8news;

/**
 * Created by ldcsb on 01/25/2017.
 */
public class FunctionalInterfaces {

    /**
     * 由此可知函数式接口只能应用于该接口只有一个抽象方法的情况中
     */
    public static void test() {
        Man man = new Man() {
            @Override
            public Man fuck() {
                return this;
            }

            @Override
            public void climax() {
                System.out.println("回归虚无");
            }
        };
        man.fuck().fuck().fuck().climax();

        Woman woman = () -> System.out.println("上天");
        woman.fuck();
    }

    private interface Man {
        Man fuck();
        void climax();
    }

    @FunctionalInterface
    private interface Woman {
        void fuck();
    }

}
