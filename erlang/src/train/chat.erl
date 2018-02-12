-module(chat).

%% API
-export([start_server/0, server/1, logon/1, logout/0, send/2, user/1]).

server_node() ->
  chat@ldc.

start_server() ->
  register(chat, spawn(chat, server, [[]])).

server(UserList) ->
  io:format("~w~n", [UserList]),
  receive
    {User, logon, Name} ->
      NewUserList = handle_logon(User, Name, UserList),
      server(NewUserList);
    {User, logout} ->
      NewUserList = handle_logout(User, UserList),
      server(NewUserList);
    {User, send, Recipient, Message} ->
      handle_message(User, Recipient, Message, UserList),
      server(UserList)
  end.

handle_logon(User, IName, UserList) ->
  case is_logged_on(IName, UserList) of
    true ->
      User ! {stop, user_logged_on_at_other_node},
      UserList;
    false ->
      User ! logged_on,
      io:format("~w logon~n", [IName]),
      [{User, IName} | UserList]
  end.

is_logged_on(IName, [{_, Name} | UserList]) ->
  IName =:= Name orelse is_logged_on(IName, UserList);
is_logged_on(_, []) ->
  false.

handle_logout(IUser, [{User, _} | UserList]) when IUser =:= User ->
  UserList;
handle_logout(IUser, [{User, _} | UserList]) ->
  [User | handle_logout(IUser, UserList)];
handle_logout(_, []) ->
  [].

handle_message(AuthorPid, RecipientName, Message, UserList) ->
  {AuthorPid, AuthorName} = search_tuple(AuthorPid, 1, UserList),
  {RecipientPid, RecipientName} = search_tuple(RecipientName, 2, UserList),
  AuthorPid ! {received, AuthorName, send_success},
  RecipientPid ! {received, AuthorName, Message}.

search_tuple(Item, N, [Tuple | _]) when element(N, Tuple) =:= Item ->
  Tuple;
search_tuple(Item, N, [_ | List]) ->
  search_tuple(Item, N, List);
search_tuple(_, _, []) ->
  nothing.

logon(Name) ->
  case whereis(chat_user) of
    undefined ->
      register(chat_user, spawn(chat, user, [Name]));
    _ ->
      already_logged_on
  end.

logout() ->
  case whereis(chat_user) of
    undefined ->
      not_logged_on;
    _ ->
      chat_user ! logout
  end.

send(Recipient, Message) ->
  case whereis(chat_user) of
    undefined ->
      not_logged_on;
    _ ->
      chat_user ! {send, Recipient, Message}
  end.

user(Name) ->
  {chat, server_node()} ! {self(), logon, Name},
  receive
    logged_on ->
      io:format("welcome ~w~n", [Name]),
      received();
    {stop, Message} ->
      io:format("~w~n", [Message]),
      exit(normal)
  end.

received() ->
  receive
    logout ->
      {chat, server_node()} ! {self(), logout},
      exit(normal);
    {send, Recipient, Message} ->
      {chat, server_node()} ! {self(), send, Recipient, Message};
    {received, Author, Message} ->
      io:format("message from ~w:~w~n", [Author, Message])
  end,
  received().