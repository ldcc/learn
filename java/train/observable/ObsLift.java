package com.ldcstorm.training.observable;

import rx.Observable;
import rx.Subscriber;


/**
 * Created by ldcsb on 10/28/2016.
 */
public class ObsLift {

    /**
     * 注释掉的代码和 lambda 代码是等价的
     * 然而无论哪个都无法进行输出
     * 现在我又不是很懂它转换的原理了
     */
    public static void test() {
        String[] player = {"shaneoyo", "Andrea", "Sherry", "GGBY", "Sure", "Id_Beat", "makkura", "Ana_Koppora", "WubWoofWolf", "Morimiya"};
        Observable.from(player)
                .compose(stringObservable -> stringObservable.lift(
                        subscriber -> new Subscriber<String>() {
                            @Override
                            public void onCompleted() {
                            }

                            @Override
                            public void onError(Throwable e) {
                            }

                            @Override
                            public void onNext(String s) {
                                subscriber.onNext(Integer.parseInt(s));
                            }
                        }))
                .subscribe(System.out::println);

//        Action1 action1 = new Action1() {
//            @Override
//            public void call(Object o) {
//                System.out.println(o);
//            }
//        };

//        Observable.from(player)
//                .compose(new LiftAll())
//                .subscribe(integer -> System.out.println(integer));

//                .lift(subscriber -> new Subscriber<String>() {
//                    @Override
//                    public void onCompleted() {
//                    }
//                    @Override
//                    public void onError(Throwable e) {
//                    }
//                    @Override
//                    public void onNext(String s) {
//                        subscriber.onNext(Integer.parseInt(s));
//                    }})
//                .subscribe(action1);
    }

    /**
     * lift() 能够接受 Operator 类型， 却不能接受向下转型的 Func1 类型
     * lift((Observable.Operator<String, Integer>) new Func1<Subscriber<? super String>, Subscriber<? super Integer>>() {
     * public Subscriber<? super Integer> call(Subscriber<? super String> subscriber) {
     * return ...;
     * }});
     * 重写的 Operator 不可以使用 Action1 代替 Subscriber
     * lift(new Observable.Operator<String, Integer>() {
     * public Subscriber<? super Integer> call(Subscriber<? super String> subscriber) {
     * return(Subscriber<? super Integer>) new Action1<Integer>() {
     * public void call(Integer integer) {}};
     * }});
     * Transformer 只有一种实现方法
     */
    private static class LiftAll implements Observable.Transformer<String, Integer> {
        @Override
        public Observable<Integer> call(Observable<String> integerObservable) {
            return integerObservable
                    .lift(subscriber -> new Subscriber<String>() {
                        @Override
                        public void onCompleted() {

                        }

                        @Override
                        public void onError(Throwable e) {

                        }

                        @Override
                        public void onNext(String s) {
                            subscriber.onNext(Integer.parseInt(s));
                            System.out.println("1");
                        }
                    });
        }
    }
}
