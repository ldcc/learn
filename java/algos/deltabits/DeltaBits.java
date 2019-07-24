package deltabits;


public class DeltaBits {
    public static int convertBits(int a, int b) {
        String sa = Integer.toBinaryString(a);
        String sb = Integer.toBinaryString(b);
        int offset = sa.length() - sb.length();
        if (offset > 0) for (int i = 0; i < Math.abs(offset); i++) sb = "0" + sb;
        else if (offset < 0) for (int i = 0; i < Math.abs(offset); i++) sa = "0" + sa;
        int bits = 0;
        for (int i = 0; i < sa.length(); i++) bits += sa.charAt(i) != sb.charAt(i) ? 1 : 0;
        return bits;
    }
}
