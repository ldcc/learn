package com.ldcstorm.training.java8news;

/**
 * Created by ldcsb on 11/9/2016.
 */
public class DefaultMethod {
    public static void test() {
//        Fox redFox = new Fox();
//        redFox.shout("hey buddy, let the bass kick");

        Parent parent = new ParentImpl();
        parent.welcome();
        System.out.println(parent.getLastMessage());

        Child child = new ChildImpl();
        child.welcome();
        System.out.println(child.getLastMessage());

        parent = new OverridingParent();
        parent.welcome();
        System.out.println(parent.getLastMessage());

        child = new OverridingChild();
        child.welcome();
        System.out.println(child.getLastMessage());
    }


    private static class Fox implements Mouth {
        @Override
        public void shout(String s) {
            System.out.println(s);
            hyperspaceShout();
        }
    }

    private interface Mouth {
        void shout(String s);

        default void hyperspaceShout() {
            System.out.println(
                    "\nO-oooooooooo AAAAE-A-A-I-A-U-" +
                            "\nJO-oooooooooooo AAE-O-A-A-U-U-A-" +
                            "\nE-eee-ee-eee AAAAE-A-E-I-E-A-" +
                            "\nJO-ooo-oo-oo-oo EEEEO-A-AAA-AAAA");
        }
    }

    private interface Parent {

        void message(String msg);

        default void welcome() {
            message("Parent: Hi!");
        }

        String getLastMessage();
    }

    private static class ParentImpl implements Parent {

        private String msg;

        @Override
        public void message(String msg) {
            this.msg = msg;
        }

        @Override
        public String getLastMessage() {
            return msg;
        }
    }

    private static class OverridingParent extends ParentImpl {

        @Override
        public void welcome() {
            message("Class Parent: Hi!");
        }
    }

    private interface Child extends Parent {
        @Override
        default void welcome() {
            message("Child: Hi!");
        }
    }

    private static class ChildImpl implements Child {

        private String msg;

        @Override
        public void message(String msg) {
            this.msg = msg;
        }

        @Override
        public String getLastMessage() {
            return msg;
        }
    }

//    private static class OverridingChild extends ChildImpl implements Child {
//    private static class OverridingChild extends ParentImpl implements Child {
    private static class OverridingChild extends OverridingParent implements Child {
    }
}
