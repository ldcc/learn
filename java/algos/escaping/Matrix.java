package escaping;

public class Matrix {
    public static <T extends Exception> void enter() throws T {
        throw (T) new Neo();
    }
}


