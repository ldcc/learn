package thsync;

import java.io.*;

public class IOStuff {

	private BufferedInputStream bis;
	private BufferedOutputStream bos;
	
	private File origin = new File("origin.txt");
	private File output = new File("output.txt");
	
	private byte [] b = new byte[1024];
	private int temp;
	
	synchronized void InputStuff() throws IOException {
		int n = 0;
		try {
			bis = new BufferedInputStream(new FileInputStream(origin));
			while (temp != -1) {
				temp = bis.read(b);
				if (temp != -1) System.out.println("读取第" + ++n + "次");
				this.notify();
				this.wait();
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			bis.close();
		}
	}
	
	synchronized void OutputStuff() throws IOException {
		int n = 0;
		try {
			bos = new BufferedOutputStream(new FileOutputStream(output));
			while (temp != -1) {
				bos.write(b, 0, temp);
				System.out.println("写入第" + ++n + "次");
				this.notify();
				this.wait();
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			bos.close();
		}
	}
}
