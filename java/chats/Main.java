package chats;

import java.io.IOException;
import java.net.Socket;

public class Main {

    public static void main(String[] args) {
        try {
            new ChatSocket().setSocketConnectListener(socketConnectListener);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static final ChatSocket.SocketConnectListener socketConnectListener = () -> {
        while (true) {
            try {
                Socket socket = ChatSocket.getServerSocket().accept();
                ChatUser chatUser = new ChatUser(socket.getLocalAddress().getHostName(), socket);
                ChatManager.getChatManager().addUser(chatUser);
                chatUser.start();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    };
}
