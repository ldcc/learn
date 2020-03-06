package chats;

import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;

/*
 * Created by ldcsb on 11/30/2016.
 */
public class ChatUser extends Thread {

    private Socket socket;

    public ChatUser(String name, Socket socket) throws IOException {
        super(name);
        this.socket = socket;
        System.out.println(name + " 连接到服务器");
        send("你已经连接到本服务器了\n\r");
    }

    @Override
    public void run() {
        receive();
    }

    public void receive() {
        try {
            InputStream inputStream = this.socket.getInputStream();
            StringBuilder stringBuilder = new StringBuilder();
            while (true) {
                char t = (char) inputStream.read();
                stringBuilder.append(t);
                if (t == '\n') {
                    flush(stringBuilder.append('\r'));
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void flush(StringBuilder stringBuilder) {
        System.out.println(stringBuilder.toString().trim());
        ChatManager.getChatManager().publish(this, stringBuilder);
        stringBuilder.setLength(0);
    }

    public void send(String in) {
        if (socket.isConnected()) {
            try {
                socket.getOutputStream().write((this.getName() + ":" + in).getBytes("GBK"));
            } catch (IOException e) {
                e.printStackTrace();
            }
        } else {
            try {
                socket.shutdownInput();
                socket.shutdownOutput();
                socket.close();
                ChatManager.getChatManager().removeUser(this);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
