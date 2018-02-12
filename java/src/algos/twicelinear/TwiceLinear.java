package org.ldccc.algos.twicelinear;

import java.util.Collections;
import java.util.TreeSet;

public class TwiceLinear {

    public static int dblLinear(int n) {
        TreeSet<Integer> u = new TreeSet<>(Collections.singletonList(1));
        for (int i = 0; i < n; i++) handle(u, u.pollFirst());
        return u.isEmpty() ? 0 : u.first();
    }

    private static void handle(TreeSet<Integer> u, int x) {
        u.add(x * 2 + 1);
        u.add(x * 3 + 1);
    }
}
