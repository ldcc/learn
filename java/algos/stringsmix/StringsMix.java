package stringsmix;


import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class StringsMix {
    public static String mix(String s1, String s2) {
        Map<Character, String> map = new HashMap<>();
        if (s1 != null && s1.length() != 0) {
            char[] chars1 = s1.trim().toCharArray();
            Map<Character, String> map1 = getMap(chars1);
            if (map1.size() > 0) {
                map1.forEach(map::put);
            }
        }
        if (s2 != null && s2.length() != 0) {
            char[] chars2 = s2.trim().toCharArray();
            Map<Character, String> map2 = getMap(chars2);
            if (map2.size() > 0) {
                map2.forEach((c, s) -> map.put(c, map.containsKey(c)
                        ? map.containsValue(s) ? "=:" + s + "/" : s.length() < map.get(c).length()
                        ? "1:" + map.get(c) + "/" : "2:" + s + "/" : "2:" + s + "/"));
            }
        }
        StringBuilder result = new StringBuilder();
        if (map.size() > 0) {
            map.forEach((c, s) -> {
                if (s.charAt(0) != '=' && s.charAt(0) != '2' && s.charAt(0) != '1') {
                    map.put(c, "1:" + s + "/");
                }
            });
            String[] str = new String[map.size()];
            int index = 0;
            for (String s : map.values()) str[index++] = s;
            for (int i = 0; i < str.length; i++) {
                for (int j = i + 1; j < str.length; j++) {
                    if (str[i].length() < str[j].length()) {
                        switchS(str, i, j);
                    } else if (str[i].length() == str[j].length()) {
                        if (str[i].charAt(0) > str[j].charAt(0)) {
                            switchS(str, i, j);
                        } else if (str[i].charAt(0) == str[j].charAt(0) && str[i].charAt(2) > str[j].charAt(2)) {
                            switchS(str, i, j);
                        }
                    }
                }
            }
            for (String s : str) result.append(s);
        }
        return result.length() == 0 ? result.toString() : result.substring(0, result.length() - 1);
    }

    private static Map<Character, String> getMap(char[] chars) {
        Map<Character, String> map = new HashMap<>();
        Set<Character> set = new HashSet<>();
        for (char c : chars) if (Character.isLowerCase(c)) map.put(c, set.add(c) ? "" + c : map.get(c) + c);
        set.forEach(c -> map.remove(map.get(c).equals(c + "") ? c : ' '));
        return map;
    }

    private static void switchS(String[] str, int i, int j) {
        String t = str[i];
        str[i] = str[j];
        str[j] = t;
    }
}
