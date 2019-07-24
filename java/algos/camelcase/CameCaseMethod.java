package camelcase;

import java.util.stream.Stream;

public class CameCaseMethod {
    //    lowercase translate to uppercase -> lower = lower - 32
    public static String camelCase(final String string) {
        return string == null || string.length() == 0 ? "" :
                Stream.of(string.trim().split(" +"))
                        .map(s -> s.length() == 1 ? s : (char) (s.charAt(0) - 32) + s.substring(1, s.length()))
                        .reduce((s, s2) -> s + s2)
                        .orElse("");
    }
}
