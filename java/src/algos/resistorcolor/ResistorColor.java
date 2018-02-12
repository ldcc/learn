package org.ldccc.algos.resistorcolor;

import java.util.stream.Stream;

public class ResistorColor {

    private static final String[] COLOR = {"black ", "brown ", "red ", "orange ", "yellow ", "green ", "blue ", "violet ", "gray ", "white "};

    public static String encodeResistorColors(String ohmsString) {
        String[] strs = ohmsString.replace("ohms", "").trim().split("\\.");
        if (strs.length == 1) {
            String str = strs[0];
            if (str.endsWith("k")) {
                str = str.replace("k", "").concat("000");
            } else if (str.endsWith("M")) {
                str = str.replace("M", "").concat("000000");
            }
            strs[0] = str;
        } else if (strs.length > 1) {
            String str = strs[strs.length - 1];
            if (str.endsWith("k")) {
                str = str.replace("k", "");
                int len = 3 - str.length();
                for (int i = 0; i < len; i++) {
                    str = str.concat("0");
                }
            } else if (str.endsWith("M")) {
                str = str.replace("M", "");
                int len = 6 - str.length();
                for (int i = 0; i < len; i++) {
                    str = str.concat("0");
                }
            }
            strs[strs.length - 1] = str;
        }
        String ohms = Stream.of(strs)
                .reduce((s, s2) -> s + s2)
                .orElse("");
        return String.format("%s %s %s gold",
                COLOR[Integer.parseInt(String.valueOf(ohms.charAt(0)))],
                COLOR[Integer.parseInt(String.valueOf(ohms.charAt(1)))],
                COLOR[ohms.length() - 2]);
    }

    public static String encodeResistorColors2(String ohmsString) {
        String str = ohmsString.replace("ohms", "").trim();
        int multiplier = str.endsWith("k") ? 1000 : str.endsWith("M") ? 1000000 : 1;
        str = "" + (int) (Double.parseDouble(str.endsWith("k") || str.endsWith("M") ? str.substring(0, str.length() - 1) : str) * multiplier);
        return COLOR[str.charAt(0) - 48] + COLOR[str.charAt(1) - 48] + COLOR[str.length() - 2] + "gold";
    }

}
