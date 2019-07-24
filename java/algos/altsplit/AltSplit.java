package altsplit;

public class AltSplit {
    public static String encrypt(final String text, final int n) {
        if (n <= 0) return text;
        String l = "", r = "";
        char[] chars = text.toCharArray();
        for (int j = 0; j < chars.length; j++) {
            if (j % 2 == 0) r += String.valueOf(chars[j]);
            else l += String.valueOf(chars[j]);
        }
        return encrypt(l + r, n - 1);
    }

    public static String decrypt(final String encryptedText, final int n) {
        if (n <= 0) return encryptedText;
        String l = encryptedText.substring(0, encryptedText.length() / 2);
        String r = encryptedText.substring(encryptedText.length() / 2);
        String source = "";
        for (int i = 0; i < r.length(); i++) {
            source += r.charAt(i);
            if (i < l.length()) source += l.charAt(i);
        }
        return decrypt(source, n - 1);
    }
}
