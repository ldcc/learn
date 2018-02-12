package org.ldccc.algos.bindservice;

public class ServiceDemo {
	
	private InterfaceDemo.BinderDemo stub = new InterfaceDemo.BinderDemo() {
		@Override
		public void doSomeThing() {
			System.out.println("Hello stub");
		}
	};
	
	public InterfaceDemo.BinderDemo getServiceBinder() {
		return stub;
	}
	
	public static ServiceDemo createService() {
		return new ServiceDemo();
	}
	
}
