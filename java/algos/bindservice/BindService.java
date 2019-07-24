package bindservice;

public class BindService {
	
	static InterfaceDemo myInterface;
	
	public static void bind() {
		ServiceDemo service = ServiceDemo.createService();
		ServiceConnectionDemo.ServiceConnection conn = new ServiceConnectionDemo.ServiceConnection();
		bindService(conn, service);
		myInterface.doSomeThing();
		unBindService(conn, service);
		myInterface.doSomeThing();
	}
	
	static void bindService(ServiceConnectionDemo.ServiceConnection conn, ServiceDemo service) {
		InterfaceDemo.BinderDemo binder = service.getServiceBinder();
		myInterface = conn.onServiceConnected(binder);
	}
	
	static void unBindService(ServiceConnectionDemo.ServiceConnection conn, ServiceDemo service) {
		service = null;
		myInterface = conn.onServiceDisconnected();
		System.out.println("Your service is gone, but you will got a new interface");
	}
	
	
	
}
