package org.ldccc.algos.morse;

import java.util.HashMap;
import java.util.Map;

public class MorseCode {
    private static final Map<String, String> CODE = new HashMap<>();
    static {
        CODE.put(".-", "A");
        CODE.put("-...", "B");
        CODE.put("-.-.", "C");
        CODE.put("-..", "D");
        CODE.put(".", "E");
        CODE.put("..-.", "F");
        CODE.put("--.", "G");
        CODE.put("....", "H");
        CODE.put("..", "I");
        CODE.put(".---", "J");
        CODE.put("-.-", "K");
        CODE.put(".-..", "L");
        CODE.put("--", "M");
        CODE.put("-.", "N");
        CODE.put("---", "O");
        CODE.put(".--.", "P");
        CODE.put("--.-", "Q");
        CODE.put(".-.", "R");
        CODE.put("...", "S");
        CODE.put("-", "T");
        CODE.put("..-", "U");
        CODE.put("...-", "V");
        CODE.put(".--", "W");
        CODE.put("-..-", "X");
        CODE.put("-.--", "Y");
        CODE.put("--..", "Z");
        CODE.put(".-.-.-", ".");
    }

    public static String get(String morseCode) {
        return CODE.get(morseCode);
    }
}
