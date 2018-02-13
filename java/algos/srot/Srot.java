package org.ldccc.algos.srot;

import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Collectors;

public class Srot {

    /**
     * Srot the inner ctnnoet in dsnnieedcg oredr.
     * You have to sort the inner content of every word of a string in descending order.
     * The inner content is the content of a word without first and the last char.
     * Some examples:
     * "sort the inner content in descending order" -> "srot the inner ctonnet in dsnnieedcg oredr"
     * "wait for me" -> "wiat for me"
     * "this kata is easy" -> "tihs ktaa is esay"
     */

    public static String sortTheInnerContent(String words) {
        return Arrays.stream(words.split(" "))
                .map(source -> getSortedString(source, sortChars(source)))
                .collect(Collectors.joining(" "));
    }

    private static String getSortedString(String source, String sorted) {
        return source.charAt(0) + sorted + source.charAt(source.length() - 1);
    }

    private static String sortChars(String source) {
        String[] letter = source.substring(1, source.length() - 1).split("");
        Arrays.sort(letter, Collections.reverseOrder());
        return String.join("", letter);
    }

}
