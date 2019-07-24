package thsync;

public class MyInput implements Runnable {
	
	private IOStuff ios;
	
	public MyInput(IOStuff ios) {
		this.ios = ios;
	}
	
	@Override
	public void run() {
		try {
			ios.InputStuff();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}


}
