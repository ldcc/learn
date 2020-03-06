package chats;

import java.util.HashSet;
import java.util.Set;

/*
 * Created by ldcsb on 12/1/2016.
 */
public class ChatManager {
    private ChatManager() {
    }

    private static final ChatManager chatManager = new ChatManager();

    public static ChatManager getChatManager() {
        return chatManager;
    }

    private static final Set<ChatUser> set = new HashSet<>();

    public void addUser(ChatUser chatUser) {
        set.add(chatUser);
    }

    public void removeUser(ChatUser chatUser) {
        set.remove(chatUser);
    }

    public void publish(ChatUser chatUser, StringBuilder stringBuilder) {
        set.forEach(user -> {
            if (!user.equals(chatUser)) {
                user.send(stringBuilder.toString());
            }
        });
    }
}
