package com.ldstorm.applet;

import java.io.IOException;
import java.net.ServerSocket;

/*
 * Created by ldcsb on 11/30/2016.
 */
public class ChatSocket {
    private static ServerSocket serverSocket;

    public ChatSocket() throws IOException {
        if (serverSocket == null) {
            serverSocket = new ServerSocket(9074);
        }
    }

    public static ServerSocket getServerSocket() {
        return serverSocket;
    }

    public void setSocketConnectListener(SocketConnectListener conn) {
        conn.connect();
    }

    public interface SocketConnectListener {
        void connect();
    }
}
