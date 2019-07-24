package smallernumber;

import java.util.*;

public class Smaller {
    public static long Smaller(long n) {
        char[] chars = String.valueOf(n).toCharArray();
        Set<Integer> set = new HashSet<>();

        // your code...

        int[] nums = set.stream()
                .mapToInt(Integer::valueOf)
                .toArray();
        Arrays.sort(nums);
        for (int num : nums) {
            System.out.println(num);
        }
        return n;
    }

    private static void swap(char[] chars, int i, int j) {
        char t = chars[i];
        chars[i] = chars[j];
        chars[j] = t;
    }
}
