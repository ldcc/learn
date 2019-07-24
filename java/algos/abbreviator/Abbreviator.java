package abbreviator;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

public class Abbreviator {

    /**
     * The word i18n is a common abbreviation of internationalization the developer community use instead of typing the whole word and trying to spell it correctly.
     * Similarly, a11y is an abbreviation of accessibility.
     * Write a function that takes a string and turns any and all "words" (see below) within that string of length 4 or greater into an abbreviation following the same rules.
     * Notes:
     * A "word" is a sequence of alphabetical characters.
     * By this definition, any other character like a space or hyphen (eg. "elephant-ride") will split up a series of letters into two words (eg. "elephant" and "ride").
     * The abbreviated version of the word should have the first letter, then the number of removed characters, then the last letter (eg. "elephant ride" => "e6t r2e").
     * Example:
     * abbreviate("elephant-rides are really fun!")
     * words (^):
     * "elephant" "rides" "are" "really" "fun"
     * 123456     123     1     1234     1
     * ignore short words:
     * L          L       S     L        S
     * abbreviate:
     * "e6t"     "r3s"   "are"  "r4y"    "fun"
     * all non-word characters (*) remain in place:
     * "-"      " "    " "     " "     "!"
     * === "e6t-r3s are r4y fun!"
     */

    public void testAbbrTrim(String string) {
        Stream.of(string.split(" "))
                .flatMap(s -> Stream.of(s.split("-"))
                        .flatMap(s1 -> Stream.of(s1.getBytes()))
                )
                .forEach(bytes -> {
                    for (byte aByte : bytes) {
                        System.out.print((char) aByte);
                    }
                });
        System.out.print("\n");
    }

    public String testAbbr(String string) {
        return testSplit(string).stream()
                .map(list -> {
                    List<String> abbrList = new ArrayList<>();
                    if (list.size() > 3) {
                        abbrList.add(list.get(0));
                        abbrList.add(String.valueOf(list.size() - 2));
                        abbrList.add(list.get(list.size() - 1));
                    } else {
                        abbrList.addAll(list);
                    }
                    return abbrList;
                })
                .flatMap(List::stream)
                .reduce((s, s2) -> s + s2)
                .orElse("");
    }

    private List<List<String>> testSplit(String string) {
        List<List<String>> lists = new ArrayList<>();
        List<String> list = new ArrayList<>();
        for (byte b : string.getBytes()) {
            if ((b >= 65 && b <= 90) || (b >= 97 && b <= 122)) {
                list.add(String.valueOf((char) b));
            } else {
                lists.add(list);
                list = new ArrayList<>();
                list.add(String.valueOf((char) b));
                lists.add(list);
                list = new ArrayList<>();
            }
        }
        return lists;
    }

    public String abbreviate(String string) {
        return specSplit(string).stream()
                .map(list -> list.size() > 3 ?
                        String.valueOf(list.get(0)) + String.valueOf(list.size() - 2) + String.valueOf(list.get(list.size() - 1)) :
                        list.stream().map(String::valueOf).reduce((s1, s2) -> s1 + s2).orElse(""))
                .reduce((s1, s2) -> s1 + s2)
                .orElse("");
    }

    private List<List<Character>> specSplit(String string) {
        List<List<Character>> lists = new ArrayList<>();
        List<Character> list = new ArrayList<>();
        for (byte b : string.getBytes()) {
            list = (b >= 65 && b <= 90) || (b >= 97 && b <= 122) ? addWords(list, (char) b) : addSymbol(lists, list, (char) b);
        }
        lists.add(list);
        return lists;
    }

    private List<Character> addWords(List<Character> list, char c) {
        list.add(c);
        return list;
    }

    private List<Character> addSymbol(List<List<Character>> lists, List<Character> list, char c) {
        lists.add(list);
        list = new ArrayList<>();
        list.add(c);
        lists.add(list);
        return new ArrayList<>();
    }


}
