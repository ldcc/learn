package anagrams;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class Anagrams {

    private static final List<String> dictionary = new ArrayList<>();

    public BigInteger listPosition(String word) {
        if (word == null || word.length() == 0 || word.length() > 26) {
            return BigInteger.ZERO;
        }

        setDictionary("", word);

        dictionary.sort(String::compareTo);

        System.out.println(dictionary);
//        System.out.println(dictionary.size());
        System.out.println(getDictLen(word.length(), BigInteger.ONE));

        for (int i = 0; i < dictionary.size(); i++) {
            if (dictionary.get(i).equals(word)) return BigInteger.valueOf(i + 1);
        }
        return BigInteger.ZERO;
    }

    private static BigInteger getDictLen(int num, BigInteger result) {
        return num == 1 ? result : getDictLen(num - 1, result.multiply(BigInteger.valueOf(num)));
    }

    private static void setDictionary(String prefix, String suffix) {
        if (suffix == null) {
            return;
        }

        if (suffix.length() == 0 && !dictionary.contains(prefix)) {
            dictionary.add(prefix);
        }

        char[] charArr = suffix.toCharArray();
        for (int i = 0; i < charArr.length; i++) {
            String temPrefix = prefix + charArr[i];
            String temSuffix = suffix.substring(0, i) + suffix.substring(i + 1, suffix.length());
            setDictionary(temPrefix, temSuffix);
        }
    }

}
