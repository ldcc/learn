package compiler.tiny.three.pass;

import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

class Simulator {
// "IM n"     // load the constant value n into R0
// "AR n"     // load the n-th input argument into R0
// "SW"       // swap R0 and R1
// "PU"       // push R0 onto the stack
// "PO"       // pop the top value off of the stack into R0
// "AD"       // add R1 to R0 and put the result in R0
// "SU"       // subtract R1 from R0 and put the result in R0
// "MU"       // multiply R0 by R1 and put the result in R0
// "DI"       // divide R0 by R1 and put the result in R0
	public static int simulate(List<String> asm, int... argv) {
		int r0 = 0;
		int r1 = 0;
		Deque<Integer> stack = new LinkedList<>();
		for (String ins : asm) {
			String code = ins.replaceAll("\\s+[0-9]+", "");
			System.out.println("R0:"+r0+", R1:"+r1+", INS:"+ins+",        STACK:"+stack);
			switch (code) {
				case "IM": r0 = Integer.parseInt(ins.substring(2).trim()); break;
				case "AR": r0 = argv[Integer.parseInt(ins.substring(2).trim())]; break;
				case "SW": int tmp = r0; r0 = r1; r1 = tmp; break;
				case "PU": stack.addLast(r0); break;
				case "PO": r0 = stack.removeLast(); break;
				case "AD": r0 += r1; break;
				case "SU": r0 -= r1; break;
				case "MU": r0 *= r1; break;
				case "DI": r0 /= r1; break;
			}
		}
		return r0;
	}
}