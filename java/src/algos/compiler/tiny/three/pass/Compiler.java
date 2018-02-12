package org.ldccc.algos.compiler.tiny.three.pass;

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

	private Ast judgeOp(String token) {
		if (isNumber(token)) {
			return new UnOp("imm", Integer.parseInt(token));
		} else {
			return new UnOp("arg", getArgIndex(token));
		}
	}

	private Ast pass1Iter(Deque<String> tokens) {
		String token = tokens.pop();
		return isAS(token) || isMD(token) ? new BinOp(token, pass1Iter(tokens), pass1Iter(tokens)) : judgeOp(token);
	}

//	private Ast pass1Iter(Deque<String> tokens) {
//		Ast last;
//		String token = tokens.peek();
//		if (token.equals("(")) {
//			Deque<String> sonTokens = getArgs(tokens, "(", ")");
//			sonTokens.add("$");
//			last = pass1Iter(sonTokens);
//		} else {
//			last = judgeOp(tokens, token);
//		}
//
////		such op get only $ or symbol
//		String op = tokens.pop();
//
//		return isAS(op) || isMD(op) ? new BinOp(op, last, pass1Iter(tokens)) : last;
//	}

	private Deque<String> parse(Deque<String> tokens) {
		Deque<String> stack1 = new LinkedList<>();
		Deque<String> stack2 = new LinkedList<>();
		while (!tokens.isEmpty()) {
			String token = tokens.pollLast();
			if (isNumber(token) || isSymbol(token)) {
				stack2.push(token);
			} else if (token.equals(")")) {
				stack1.push(token);
			} else if (token.equals("(")) {
				String tem = stack1.pop();
				while (!tem.equals(")")) {
					stack2.push(tem);
					tem = stack1.pop();
				}
			} else if (isMD(token)) {
				stack1.push(token);
			} else if (isAS(token)) {
				String tem = stack1.peek();
				while (tem != null && isMD(tem)) {
					stack2.push(stack1.pop());
					tem = stack1.peek();
				}
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
		String token;	tokens.pop();
		while (!(token = tokens.pop()).equals("]")) tem.add(token);
		args = tem.toArray(new String[0]);
		Deque<String> stack = parse(tokens);
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

	private Ast calculator(String op, int a, int b) {
		switch (op) {
			case "+":
				return new UnOp("imm", a + b);
			case "-":
				return new UnOp("imm", a - b);
			case "*":
				return new UnOp("imm", a * b);
			default:
				return new UnOp("imm", a / b);
		}
	}

	public Ast pass2(Ast ast) {
		if (ast instanceof UnOp) {
			return ast;
		} else {
			Ast a = pass2(((BinOp) ast).a());
			Ast b = pass2(((BinOp) ast).b());
			if (a.op().equals("imm") && b.op().equals("imm")) {
				return calculator(ast.op(), ((UnOp) a).n(), ((UnOp) b).n());
			} else {
				return new BinOp(ast.op(), a, b);
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
			List<String> v1 = pass3(bin.a());
			List<String> v2 = pass3(bin.b());

			List<String> tem = new ArrayList<>();
			if (bin.b() instanceof UnOp) {
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