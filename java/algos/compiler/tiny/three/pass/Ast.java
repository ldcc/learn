package compiler.tiny.three.pass;

public interface Ast {
	String op();
}

class BinOp implements Ast {
	private String op;
	private Ast l;
	private Ast r;

	public BinOp(String op, Ast l, Ast r) {
		this.op = op;
		this.l = l;
		this.r = r;
	}

	@Override
	public String op() {
		return op;
	}

	public Ast l() {
		return l;
	}

	public Ast r() {
		return r;
	}

	@Override
	public String toString() {
		return "(" + op + " " + l + " " + r + ")";
	}

	@Override
	public int hashCode() {
		return (op.hashCode() + l.hashCode() + r.hashCode()) * 31;
	}

	@Override
	public boolean equals(Object o) {
		return o instanceof Ast && this.hashCode() == o.hashCode();
	}
}

class UnOp implements Ast {
	private String op;
	private int n;

	public UnOp(String op, int n) {
		this.op = op;
		this.n = n;
	}

	@Override
	public String op() {
		return op;
	}

	public int n() {
		return n;
	}

	@Override
	public String toString() {
		return String.valueOf(n);
	}

	@Override
	public int hashCode() {
		return (op.hashCode() + n) * 31;
	}
}
