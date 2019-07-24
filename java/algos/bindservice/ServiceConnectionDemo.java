package bindservice;

public interface ServiceConnectionDemo {
	
	InterfaceDemo onServiceConnected(InterfaceDemo.BinderDemo binder);
	InterfaceDemo onServiceDisconnected();
	
	class ServiceConnection implements ServiceConnectionDemo {
		
		@Override
		public InterfaceDemo onServiceConnected(InterfaceDemo.BinderDemo binder) {
			System.out.println("Service is binded");
			return handleService(binder);
		}

		@Override
		public InterfaceDemo onServiceDisconnected() {
			System.out.println("Service unbind now");
			return handleService(null);
		}
		
		public InterfaceDemo handleService(InterfaceDemo.BinderDemo binder) {
			try {
				return InterfaceDemo.BinderDemo.asInterface(binder);
			} catch (NullPointerException e) {
				e.printStackTrace();
				throw new NullPointerException();
			}
		}
		
	}
}
