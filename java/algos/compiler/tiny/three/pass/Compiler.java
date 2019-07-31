package compiler.tiny.three.pass;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Compiler {
	public List<String> compile(String prog) {
		return pass3(pass2(pass1(prog)));
	}

//	private Deque<String> getArgs(Deque<String> tokens, String startOf, String endOf) {
//		Deque<String> args = new LinkedList<>();
//		Stack<String> stack = new Stack<>();
//		do {
//			String token = tokens.pop();
//			if (token.equals(endOf)) {
//				stack.pop();
//			} else if (token.equals(startOf)) {
//				stack.push(token);
//			} else {
//				args.add(token);
//			}
//		} while (!stack.empty());
//		return args;
//	}

	private boolean isAS(String token) {
		return token.matches("[+-]");
	}

	private boolean isMD(String token) {
		return token.matches("[*/]");
	}

	private boolean isNumber(String token) {
		return token.matches("[0-9]*");
	}

	private boolean isSymbol(String token) {
		return token.matches("[\\w]*");
	}

//	private String[] getArgs(Deque<String> tokens) {
//		List<String> args = new ArrayList<>();
//		String token;
//		tokens.pop();
//		while (!(token = tokens.pop()).equals("]")) args.add(token);
//		return args.toArray(new String[0]);
//	}

	private int getArgIndex(String arg) {
		for (int i = 0; i < args.length; i++) if (args[i].equals(arg)) return i;
		return 0;
	}

	private Ast determineOp(String token) {
		if (isNumber(token)) {
			return new UnOp("imm", Integer.parseInt(token));
		} else {
			return new UnOp("arg", getArgIndex(token));
		}
	}

	private Ast pass1Iter(Deque<String> tokens) {
		String token = tokens.pop();
		return isAS(token) || isMD(token) ? new BinOp(token, pass1Iter(tokens), pass1Iter(tokens)) : determineOp(token);
	}

//	private Ast pass1Iter(Deque<String> tokens) {
//		Ast last;
//		String token = tokens.peek();
//		if (token.equals("(")) {
//			Deque<String> sonTokens = getArgs(tokens, "(", ")");
//			sonTokens.add("$");
//			last = pass1Iter(sonTokens);
//		} else {
//			last = determineOp(tokens, token);
//		}
//
////		such op get only $ or symbol
//		String op = tokens.pop();
//
//		return isAS(op) || isMD(op) ? new BinOp(op, last, pass1Iter(tokens)) : last;
//	}

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
			}
		}
		while (!stack1.isEmpty()) stack2.push(stack1.pop());
		return stack2;
	}

	private String[] args;

	/**
	 * Returns an un-optimized AST
	 */

	public Ast pass1(String prog) {
		Deque<String> tokens = tokenize(prog);
		List<String> tem = new ArrayList<>();
		String token;
		tokens.pop();
		while (!(token = tokens.pop()).equals("]")) tem.add(token);
		args = tem.toArray(new String[0]);
		Deque<String> stack = parsing(tokens);
		return pass1Iter(stack);
	}

//	private Ast pass2Iter(Ast ast) {
//		if (ast instanceof Atom) {
//			return ast;
//		} else {
//			Ast a = pass2Iter(((BinOp) ast).a());
//			Ast b = pass2Iter(((BinOp) ast).b());
//			if (a.op().equals("imm") && b.op().equals("imm")) {
//				return new Atom("imm", ((Atom) a).n() + ((Atom) b).n());
//			} else {
//				return ast;
//			}
//		}
//	}

	/**
	 * Returns an AST with constant expressions reduced
	 */

	private Ast calculate(String op, int l, int r) {
		switch (op) {
			case "+":
				return new UnOp("imm", l + r);
			case "-":
				return new UnOp("imm", l - r);
			case "*":
				return new UnOp("imm", l * r);
			default:
				return new UnOp("imm", l / r);
		}
	}

	public Ast pass2(Ast ast) {
		if (ast instanceof UnOp) {
			return ast;
		} else {
			Ast l = pass2(((BinOp) ast).l());
			Ast r = pass2(((BinOp) ast).r());
			if (l.op().equals("imm") && r.op().equals("imm")) {
				return calculate(ast.op(), ((UnOp) l).n(), ((UnOp) r).n());
			} else {
				return new BinOp(ast.op(), l, r);
			}
		}
	}

	/**
	 * Returns assembly instructions
	 */

	private String opSelector(String op) {
		switch (op) {
			case "+":
				return "AD";
			case "-":
				return "SU";
			case "*":
				return "MU";
			default:
				return "DI";
		}
	}

	private String unSelector(UnOp un) {
		return (un.op().equals("imm") ? "IM " : "AR ") + un.n();
	}

	public List<String> pass3(Ast ast) {
		if (ast instanceof UnOp) {
			return Collections.singletonList(unSelector((UnOp) ast));
		} else {
			BinOp bin = (BinOp) ast;
			List<String> v1 = pass3(bin.l());
			List<String> v2 = pass3(bin.r());

			List<String> tem = new ArrayList<>();
			if (bin.r() instanceof UnOp) {
				tem.addAll(v1);
				tem.add("SW");
				tem.addAll(v2);
				tem.add("SW");
				tem.add(opSelector(ast.op()));
				return tem;
			} else {
				tem.addAll(v1);
				tem.add("PU");
				tem.addAll(v2);
				tem.add("SW");
				tem.add("PO");
				tem.add(opSelector(ast.op()));
				return tem;
			}
		}
	}

	private static Deque<String> tokenize(String prog) {
		Deque<String> tokens = new LinkedList<>();
		Pattern pattern = Pattern.compile("[-+*/()\\[\\]]|[a-zA-Z]+|\\d+");
		Matcher m = pattern.matcher(prog);
		while (m.find()) tokens.add(m.group());
		tokens.add("$"); // end-of-stream
		return tokens;
	}
}