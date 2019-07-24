package interactive.interpreter;

import javafx.util.Pair;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Interpreter {

  private List<Pair<String, Ast>> env = new ArrayList<>();

  private Ast lookup(String key) {
    for (int i = env.size() - 1; i >= 0; i--) {
      if (env.get(i).getKey().equals(key)) {
        return env.get(i).getValue();
      }
    }
    throw new IllegalArgumentException("Unknown identifier " + key);
  }

  private boolean contain(String key) {
    for (Pair<String, Ast> pair : env) {
      if (pair.getKey().equals(key)) {
        return true;
      }
    }
    return false;
  }

  private Double extEnv(String key, Ast val) {
    env.add(new Pair<>(key, val));
    return calcIter(val);
  }

  private boolean strikeOut(String key) {
    for (int i = env.size() - 1; i >= 0; i--) {
      if (env.get(i).getKey().equals(key)) {
        return env.remove(env.get(i));
      }
    }
    return false;
  }

  private void extEnv(String[] args, Ast[] asts) {
    for (int i = 0; i < args.length; i++) {
      extEnv(args[i], (asts[i]));
    }
  }

  private void strikeOut(String[] args) {
    for (String arg : args) {
      strikeOut(arg);
    }
  }

  private Map<String, Closure> funMap = new HashMap<>();

  private boolean isAS(String token) {
    return token.matches("[-+]");
  }

  private boolean isMD(String token) {
    return token.matches("[*/%]");
  }

  private boolean isAssign(String token) {
    return token.equals("=");
  }

  private boolean isNumber(String token) {
    return token.matches("\\d+.?\\d*");
  }

  private boolean isSymbol(String token) {
    return token.matches("[\\w]*");
  }

  private boolean isFunction(String token) {
    return token.equals("=>");
  }

  private Deque<String> parsing(Deque<String> tokens) {
    Deque<String> stack1 = new LinkedList<>();
    Deque<String> stack2 = new LinkedList<>();
    while (!tokens.isEmpty()) {
      String token = tokens.pollLast();
      if (isNumber(token) || isSymbol(token)) {
        stack2.push(token);
      } else if (token.equals(")")) {
        stack1.push(token);
      } else if (token.equals("(")) {
        while (!stack1.peek().equals(")")) stack2.push(stack1.pop());
        stack1.pop();
      } else if (isMD(token)) {
        stack1.push(token);
      } else if (isAS(token)) {
        while (!stack1.isEmpty() && isMD(stack1.peek())) stack2.push(stack1.poll());
        stack1.push(token);
      } else if (isAssign(token)) {
        while (!stack1.isEmpty() && !stack1.peek().equals(")")) stack2.push(stack1.pop());
        stack1.push(token);
      } else if (isFunction(token)) {
        while (!stack1.isEmpty()) stack2.push(stack1.pop());
        stack2.push(token);
      }
    }
    while (!stack1.isEmpty()) stack2.push(stack1.pop());
    return stack2;
  }

  private Ast closure(Deque<String> tokens) {
    String op = tokens.pop();
    String name = tokens.pop();
    String[] args = getArgs(tokens).split(" ");
    Ast expr = getAst(tokens);
    if (checkFnArgs(args) && checkFnExpr(args, expr)) {
      return new Closure(op, name, args, expr);
    } else {
      throw new IllegalArgumentException("Unknown identifier " + Arrays.toString(args));
    }
  }

  private Ast getAst(Deque<String> tokens) {
    String token = tokens.pop();
    if (funMap.containsKey(token)) {
      return new Func(token, getAsts(tokens, funMap.get(token).args.length));
    } else if (token.matches("[-+*/%=]")) {
      return new Op(token, getAst(tokens), getAst(tokens));
    } else {
      return getAtom(token);
    }
  }

  private Ast[] getAsts(Deque<String> tokens, int len) {
    Ast[] asts = new Ast[len];
    for (int i = 0; i < asts.length; i++) asts[i] = getAst(tokens);
    return asts;
  }

  private String getArgs(Deque<String> tokens) {
    String token = tokens.pop();
    if (token.equals("=>")) {
      return " ";
    } else {
      return token + " " + getArgs(tokens);
    }
  }

  private Ast getAtom(String token) {
    if (isNumber(token)) {
      return new Atom("const", token);
    } else {
      return new Atom("param", token);
    }
  }

  private boolean checkFnArgs(String[] args) {
    Set<String> set = new HashSet<>();
    Collections.addAll(set, args);
    return set.size() == args.length;
  }

  private boolean checkFnExpr(String[] args, Ast expr) {
    if (expr.op().matches("[-+*/%=]")) {
      return checkFnExpr(args, ((Op) expr).car) && checkFnExpr(args, ((Op) expr).cdr);
    } else {
      String val = ((Atom) expr).v;
      return isNumber(val) || Arrays.stream(args).anyMatch(arg -> arg.equals(val));
    }
  }

  private Double calcIter(Ast ast) {
    if (ast == null) return null;
    if (ast.op().matches("[-+*/%]")) {
      Op bin = (Op) ast;
      Double car = calcIter(bin.car);
      Double cdr = calcIter(bin.cdr);
      switch (bin.op()) {
        case "+":
          return car + cdr;
        case "-":
          return car - cdr;
        case "*":
          return car * cdr;
        case "/":
          return car / cdr;
        case "%":
          return car % cdr;
        default:
          return null;
      }
    } else if (ast.op().equals("=")) {
      Op bin = (Op) ast;
      String key = ((Atom) bin.car).v;
      if (funMap.containsKey(key)) {
        throw new IllegalArgumentException("Identifier with conflicts");
      } else {
        return extEnv(key, bin.cdr);
      }
    } else if (ast.op().equals("fn")) {
      Closure closure = (Closure) ast;
      if (contain(closure.name)) {
        throw new IllegalArgumentException("Function with conflicts");
      } else {
        funMap.put(closure.name, closure);
      }
      return null;
    } else if (funMap.containsKey(ast.op())) {
      Func func = (Func) ast;
      Closure closure = funMap.get(ast.op());
      extEnv(closure.args, func.asts);
      Double val = calcIter(closure.exp);
      strikeOut(closure.args);
      return val;
    } else {
      Atom atom = (Atom) ast;
      switch (atom.op()) {
        case "const":
          return Double.valueOf(atom.v);
        case "param":
          return calcIter(lookup(atom.v));
        default:
          return null;
      }
    }
  }

  public Double input(String input) {
    if (input.trim().isEmpty()) return null;
    System.out.println(input);
    Deque<String> tokens = tokenize(input);

    Deque<String> polish = parsing(tokens);
    System.out.println(polish);

    Ast ast = polish.peek().equals("fn") ? closure(polish) : getAst(polish);
    System.out.println(ast);

    if (!polish.isEmpty()) {
      throw new IllegalArgumentException("Mismatch expression");
    } else {
      return calcIter(ast);
    }
  }


  interface Ast {
    String op();
  }

  private class Closure implements Ast {
    String op;
    String name;
    String[] args;
    Ast exp;

    public Closure(String op, String name, String[] args, Ast exp) {
      this.op = op;
      this.name = name;
      this.args = args;
      this.exp = exp;
    }

    @Override
    public String op() {
      return op;
    }

    @Override
    public String toString() {
      return name + Arrays.toString(args) + " => " + exp;
    }
  }

  private class Func implements Ast {
    String op;
    Ast[] asts;

    public Func(String op, Ast[] asts) {
      this.op = op;
      this.asts = asts;
    }

    @Override
    public String op() {
      return op;
    }

    @Override
    public String toString() {
      return op + Arrays.toString(asts);
    }
  }

  private class Op implements Ast {
    String op;
    Ast car;
    Ast cdr;

    public Op(String op, Ast car, Ast cdr) {
      this.op = op;
      this.car = car;
      this.cdr = cdr;
    }

    @Override
    public String op() {
      return op;
    }

    @Override
    public String toString() {
      return "(" + op + " " + car + " " + cdr + ")";
    }
  }

  private class Atom implements Ast {
    String op;
    String v;

    public Atom(String op, String v) {
      this.op = op;
      this.v = v;
    }

    @Override
    public String op() {
      return op;
    }

    @Override
    public String toString() {
      return String.valueOf(v);
    }
  }

  private static Deque<String> tokenize(String input) {
    Deque<String> tokens = new LinkedList<>();
    Pattern pattern = Pattern.compile("=>|[-+*/%=()]|[A-Za-z_][A-Za-z0-9_]*|[0-9]*(\\.?[0-9]+)");
    Matcher m = pattern.matcher(input);
    while (m.find()) tokens.add(m.group());
    return tokens;
  }
}