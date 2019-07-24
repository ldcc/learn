package validbraces;


import java.util.*;

public class ValidBraces {
    private static final Map<Character, Character> valid = new HashMap<>();

    static {
        valid.put('(', ')');
        valid.put('[', ']');
        valid.put('{', '}');
    }

    public static boolean isValidBeta(String braces) {
        List<String> list = split(braces);
        for (String s : list) {
            System.out.println(s);
            if (!valid.containsKey(s.charAt(0))) return false;
            if (!parse(s)) return false;
        }
        return true;
    }
//
    private static List<String> split(String braces) {
        List<String> list = new ArrayList<>();
        StringBuilder symbol = new StringBuilder();
        char fir = ' ';
        for (char c : braces.toCharArray()) {
            if (symbol.toString().equals("")) {
                fir = c;
            }
            symbol.append(c);
            if (valid.containsKey(fir) && valid.get(fir) == c) {
                list.add(symbol.toString());
                symbol = new StringBuilder();
            }

        }
        if (!symbol.toString().equals("")) list.add(symbol.toString());
        return list;
    }

    private static boolean parse(String braces) {
        return braces.matches("\\(\\)|\\[]|\\{}") || valid.get(braces.charAt(0)) == braces.charAt(braces.length() - 1) && parse(braces.substring(1, braces.length() - 1));
    }

    public static boolean isValid(String braces) {
        Stack<Character> stack = new Stack<>();
        for (char c : braces.toCharArray()) {
            if (!stack.empty() && valid.containsKey(stack.peek()) && valid.get(stack.peek()) == c) stack.pop();
            else stack.push(c);
        }
        return stack.empty();
    }

}
