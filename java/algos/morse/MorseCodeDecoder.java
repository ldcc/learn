package org.ldccc.algos.morse;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Comparator;
import java.util.stream.Collectors;

public class MorseCodeDecoder {
    private static final String DASH = "(111)";
    private static final String PL = "(000)";
    private static final String PW = "(0000000)";

    public static String decodeMorse(String morseCode) {
        return Arrays.stream(morseCode.split(" {3}"))
                .map(s -> Arrays.stream(s.split(" "))
                        .map(MorseCode::get)
                        .collect(Collectors.joining()))
                .collect(Collectors.joining(" "));
    }

    public static String decodeBits(String bits) {
        bits = bits.substring(bits.indexOf('1'), bits.lastIndexOf('1') + 1);
        final String unit = "{" + reduce(bits) + ",}";
        return Arrays.stream(bits.split(PW + unit))
                .map(word -> Arrays.stream(word.split(PL + unit))
                        .map(letter -> Arrays.stream(letter.split("0+"))
                                .map(morse -> morse.matches(DASH + unit) ? "-" : ".")
                                .collect(Collectors.joining()))
                        .collect(Collectors.joining(" ")))
                .collect(Collectors.joining("   "));
    }

    private static int reduce(String bits) {
        List<String> list = new ArrayList<>();
        StringBuilder str = new StringBuilder();
        for (char c : bits.toCharArray()) {
            if (!str.toString().matches(c + "*")) {
                list.add(str.toString());
                str = new StringBuilder();
            }
            str.append(c);
        }
        list.add(str.toString());
        return list.stream().min(Comparator.comparing(String::length)).orElse("").length();
    }
}
