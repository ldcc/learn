package org.ldccc.algos.multiples;

import java.util.ArrayList;
import java.util.List;

public class Multiples {

    public int solution(int number) {
        return getNumberList(number)
                .stream()
                .filter(i -> i % 3 == 0 || i % 5 == 0)
                .reduce((i1, i2) -> i1 + i2)
                .orElse(0);
    }

    public List<Integer> getNumberList(int maxNumber) {
        List<Integer> list = new ArrayList<>();
        for (int i = 1; i < maxNumber; i++) list.add(i);
        return list;
    }
}
