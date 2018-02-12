package org.ldccc.algos.bindservice;

interface InterfaceDemo {

    void doSomeThing();

    abstract class BinderDemo implements InterfaceDemo {

        @Override
        public void doSomeThing() {
            System.out.println("Hello Binder");
        }

        public BinderDemo asBinder() {
            return new BinderDemo() {
            };
        }

        static InterfaceDemo asInterface(BinderDemo serverBinder) {
            return serverBinder != null ? serverBinder : new Proxy(serverBinder);
        }

        static class Proxy implements InterfaceDemo {

            BinderDemo remote = new BinderDemo() {
            };

            Proxy(BinderDemo binder) {
                remote = binder != null ? binder : asBinder();
            }

            BinderDemo asBinder() {
                return new BinderDemo() {
                };
            }

            @Override
            public void doSomeThing() {
                remote.doSomeThing();
                System.out.println("Hello Proxy");
            }

        }

    }
}
