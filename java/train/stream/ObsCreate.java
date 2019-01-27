package stream;

import rx.Observable;
import rx.Subscriber;

/**
 * Created by ldcsb on 10/26/2016.
 */
public class ObsCreate {

    public static void test() {
        Observable.create((Observable.OnSubscribe<String>) subscriber -> {
            subscriber.onNext("Hello RxJava");
            subscriber.onCompleted();
            subscriber.onError(new Throwable("subscribe error"));
        }).subscribe(new Subscriber<String>() {
            @Override
            public void onCompleted() {
                System.out.println("complete subscribe");
            }

            @Override
            public void onError(Throwable throwable) {
                throwable.printStackTrace();
            }

            @Override
            public void onNext(String s) {
                System.out.println(s);
            }
        });

    }


}
