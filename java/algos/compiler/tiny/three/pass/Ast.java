package compiler.tiny.three.pass;

public interface Ast {
	String op();
}

class BinOp implements Ast {
	private String op;
	private Ast a;
	private Ast b;

	public BinOp(String op, Ast a, Ast b) {
		this.op = op;
		this.a = a;
		this.b = b;
	}

	@Override
	public String op() {
		return op;
	}

	public Ast a() {
		return a;
	}

	public Ast b() {
		return b;
	}

	@Override
	public String toString() {
		return "(" + op + " " + a + " " + b + ")";
	}

	@Override
	public int hashCode() {
		return (op.hashCode() + a.hashCode() + b.hashCode()) * 31;
	}

	@Override
	public boolean equals(Object o) {
		return o instanceof Ast && this.hashCode() == o.hashCode();
	}
}

class UnOp implements Ast {
	private String op;
	private int i;

	public UnOp(String op, int i) {
		this.op = op;
		this.i = i;
	}

	@Override
	public String op() {
		return op;
	}

	public int n() {
		return i;
	}

	@Override
	public String toString() {
		return String.valueOf(i);
	}

	@Override
	public int hashCode() {
		return (op.hashCode() + i) * 31;
	}
}
