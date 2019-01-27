package algos.interactive.interpreter;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public class InterpreterTest {

  @org.junit.jupiter.api.Test
  public static void basicTests() {
    org.ldccc.algos.interactive.interpreter.Interpreter interpreter = new org.ldccc.algos.interactive.interpreter.Interpreter();

    // Basic arithmetic
    assertEquals(11, interpreter.input("4 + 2 * 3 + 1"), 0.0);
    assertEquals(2, interpreter.input("1 + 1"), 0.0);
    assertEquals(1, interpreter.input("2 - 1"), 0.0);
    assertEquals(6, interpreter.input("2 * 3"), 0.0);
    assertEquals(2, interpreter.input("8 / 4"), 0.0);
    assertEquals(3, interpreter.input("7 % 4"), 0.0);

    System.out.println("Basic arithmetic finished");

    // Variables
    assertEquals(1, interpreter.input("x = 1"), 0.0);
    assertEquals(1, interpreter.input("x"), 0.0);
    assertEquals(4, interpreter.input("x + 3"), 0.0);
    assertFail("input: 'y'", () -> interpreter.input("y"));

    System.out.println("Variables test finished");

    // Functions
    interpreter.input("fn avg x y => (x + y) / 2");
    assertEquals(3, interpreter.input("avg 4 2"), 0.0);
    assertFail("input: 'avg 7'", () -> interpreter.input("avg 7"));
    assertFail("input: 'avg 7 2 4'", () -> interpreter.input("avg 7 2 4"));

    System.out.println("Functions test finished");

    // Conflicts
    assertFail("input: 'fn x => 0'", () -> interpreter.input("fn x => 0"));
    assertFail("input: 'avg = 5'", () -> interpreter.input("avg = 5"));

    System.out.println("Conflicts test finished");
  }


  @Test
  public static void conflictsTests() {
    org.ldccc.algos.interactive.interpreter.Interpreter interpreter = new org.ldccc.algos.interactive.interpreter.Interpreter();

    assertEquals(0, interpreter.input("x = 0"), 0.0);
    interpreter.input("fn f => 1");
    assertFail("input: 'fn x => 0'", () -> interpreter.input("fn x => 0"));
    assertFail("input: 'f = 5'", () -> interpreter.input("f = 5"));
    assertEquals(1, interpreter.input("f"), 0.0);
    interpreter.input("fn f => 0");
    assertEquals(0, interpreter.input("f"), 0.0);
  }

  @Test
  public static void functionsTests() {
    org.ldccc.algos.interactive.interpreter.Interpreter interpreter = new org.ldccc.algos.interactive.interpreter.Interpreter();

    assertEquals(23, interpreter.input("x = 23"), 0.0);
    assertEquals(25, interpreter.input("y = 25"), 0.0);
    assertEquals(0, interpreter.input("z = 0"), 0.0);

    assertEquals(null, interpreter.input("fn one => 1"));
    assertEquals(null, interpreter.input("fn avg x y => (x + y) / 2"));
    assertEquals(null, interpreter.input("fn echo x => x"));
    assertFail("input: 'fn add x y => x + z'", () -> interpreter.input("fn add x y => x + z"));
    assertFail("input: 'fn add x x => x + x'", () -> interpreter.input("fn add x x => x + x"));
  }

  @Test
  public static void variablesTests() {
    org.ldccc.algos.interactive.interpreter.Interpreter interpreter = new org.ldccc.algos.interactive.interpreter.Interpreter();

    assertEquals(7, interpreter.input("x = 7"), 0.0);
    assertEquals(7, interpreter.input("x"), 0.0);
    assertEquals(10, interpreter.input("x + 3"), 0.0);
    assertFail("input: 'y'", () -> interpreter.input("y"));
    assertEquals(12, interpreter.input("y = x + 5"), 0.0);
    assertEquals(12, interpreter.input("y"), 0.0);
    assertEquals(713, interpreter.input("x = y = 713"), 0.0);
    assertEquals(713, interpreter.input("x"), 0.0);
    assertEquals(713, interpreter.input("y"), 0.0);
    assertEquals(40, interpreter.input("x = 29 + (y = 11)"), 0.0);
    assertEquals(40, interpreter.input("x"), 0.0);
    assertEquals(11, interpreter.input("y"), 0.0);
  }

  @Test
  public static void interpreterTest() {
    org.ldccc.algos.interactive.interpreter.Interpreter interpreter = new org.ldccc.algos.interactive.interpreter.Interpreter();

    assertEquals(null, interpreter.input(""));
    assertEquals(null, interpreter.input(" "));
    assertEquals(9, interpreter.input("9"), 0.0);
    assertFail("input: '1 2'", () -> interpreter.input("1 2"));
    assertFail("input: '1two'", () -> interpreter.input("1two"));
    assertEquals(2, interpreter.input("1 + 1"), 0.0);
    assertEquals(4, interpreter.input("2+2"), 0.0);
    assertEquals(1, interpreter.input("2 - 1"), 0.0);
    assertEquals(-2, interpreter.input("4-6"), 0.0);
    assertEquals(6, interpreter.input("2 * 3"), 0.0);
    assertEquals(2, interpreter.input("8 / 4"), 0.0);
    assertEquals(3, interpreter.input("7 % 4"), 0.0);
  }

  private static void assertFail(String msg, Runnable runnable) {
    try {
      runnable.run();
      fail(msg);
    } catch (Exception e) {
      // Ok
    }
  }
}