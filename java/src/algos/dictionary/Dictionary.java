package org.ldccc.algos.dictionary;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class Dictionary {
    private final String[] words;

    public Dictionary(String[] words) {
        this.words = words;
    }

    public String findMostSimilar(String to) {
        Map<String, Integer> frequency = new HashMap<>();
//        Map<String, Integer> fq = new HashMap<>();
        for (String word : words) {
            for (int i = 0; i < to.length(); i++) {
                if (word.contains(to.substring(i, i + 1))) {
                    int times = 0;
                    for (int j = i + 1; j <= to.length(); j++) {
                        if (word.contains(to.substring(i, j))) {
                            times += 2;
                        }
                    }
                    if (!frequency.containsKey(word) || frequency.get(word) < times) {
                        frequency.put(word, times);
//                        fq.put(word, times);
                    }
                }
            }
        }
//        fq.forEach((s, i) -> fq.forEach((s1, i1) -> {
//            if (!s.contains(s1)) fq.put(s, i + i1);
//        }));
//        System.out.println(fq);
        String correct = "";
        int maxAbs = Integer.MIN_VALUE, currAbs = 0;
        for (String s : frequency.keySet()) {
            currAbs = frequency.get(s) - s.length() - Math.abs(s.length() - to.length());
            if (currAbs > maxAbs) {
                correct = s;
                maxAbs = currAbs;
            }
        }
        return correct;
//        frequency.forEach((s, i) -> frequency.put(s, frequency.get(s) - s.length() - Math.abs(s.length() - to.length())));
//        System.out.println(frequency);
//        return frequency.keySet().stream().max(Comparator.comparing(frequency::get)).orElse("");
    }

    public void debug() {
        for (String word : words) {
            char[] chars = word.toCharArray();
            Arrays.sort(chars);
            System.out.println(chars);
        }
    }
}
