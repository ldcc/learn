package thsync;

public class MyOutput implements Runnable {

	private IOStuff ios;
	
	public MyOutput(IOStuff ios) {
		this.ios = ios;
	}
	
	@Override
	public void run() {
		try {
			ios.OutputStuff();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
}
