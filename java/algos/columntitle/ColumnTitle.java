package columntitle;

public class ColumnTitle {

    public static String getColumnTitle(int num) {
        if (num <= 0) throw new IllegalArgumentException("The Number can't be zero");
        String result = num <= 26 ? String.valueOf((char) (num + 64)) : num % 26 == 0 ? getColumnTitle(num / 26 - 1) : getColumnTitle(num / 26);
        return num > 26 ? num % 26 == 0 ? result + getColumnTitle(26) : result + getColumnTitle(num % 26) : result;
    }
}
