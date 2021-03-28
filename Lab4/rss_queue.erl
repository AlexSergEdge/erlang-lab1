-module(rss_queue).
-compile(export_all).
-define(TIMEOUT,10000).
-include("logging.hrl").
% -import(rss_parse, [get_feed_items/1, compare_feed_items/2]).

init([])->
    start().

%% @doc Запуск сервера, функция возвращает PID созданного процесса
start()->
    Q = [],
    spawn(?MODULE,server,[Q]).

%% @doc Функция Server - цикл сервера процесса очереди RSS
server(Q)->
    receive
        %% @doc Добавление нового элемента в очередь RSS
        {add_item, RSSItem} ->
            NewQ = add_item_to_q(RSSItem,Q),
            server(NewQ);
        %% @doc Получение всего содержимого очереди 
        {get_all, ReqPid} ->
            ReqPid ! {self(), Q},
            ?INFO("Sent rss items to ~p~n",[ReqPid]),
            server(Q);
        _Msg -> io:format("Unknown msg~p~n",[_Msg])  
    end.

%% @doc Отправка элемента в очередь
add_item(QPid, Item)->
    QPid ! {add_item,Item},
    ok.

%% @doc Извлечение элементов из ленты и отправка в очередь по порядку
add_feed(QPid, RSS2Feed) when is_pid(QPid) ->
    Items=rss_parse:get_feed_items(RSS2Feed),
    [add_item(QPid,Item) || Item <- Items],
    ?INFO("Added N=~p items from the feed to ~p ~n",[length(Items),QPid]),
    ok.

%% @doc Функция обновления элемента
add_item_to_q(NewItem,Q)->
    add_item_to_q_helper(NewItem,[],Q).

%% @doc Вспомогательная функция обновления элемента, когда прошли всю очередь
add_item_to_q_helper(NewItem,L1,[])->
    ?INFO("New item ~p ~n",[self()]),
    L1++[NewItem];
%% @doc Вспомогательная функция обновления элемента
%% Проходимся по элементам, и проверяем, новый ли item, или обновленный, или уже существующий
add_item_to_q_helper(NewItem,L1,L=[OldItem|Rest])->
    case rss_parse:compare_feed_items(OldItem,NewItem) of
        same -> 
            L1++L;
        updated -> 
            ?INFO("Updated item ~p ~n",[self()]),
            L1++Rest++[NewItem];
        different -> 
            add_item_to_q_helper(NewItem,L1++[OldItem],Rest)
    end.

%% @doc Получение списка всех элементов очереди
get_all(QPid) when is_pid(QPid)->
    QPid ! {get_all,self()},
    receive
        {QPid,Q} -> Q;
        _Msg -> {error,unknown_msg,_Msg}
    after 
        ?TIMEOUT -> {error,timeout}
    end.

%% @doc TEST
add_news(RSSFile1, RSSFile2) ->
    PID = start(),
	{XML2, _} = xmerl_scan:file(RSSFile2),
	add_feed(PID, XML2),
    {XML1, _} = xmerl_scan:file(RSSFile1),
    add_feed(PID, XML1).