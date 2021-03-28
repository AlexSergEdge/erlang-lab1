-module(rss_queue).
-compile(export_all).
-define(TIMEOUT,10000).
-include("logging.hrl").
% -import(rss_parse, [get_feed_items/1, compare_feed_items/2]).

init([])->
    start();
init([Url]) ->
    start(Url).

%% @doc Запуск сервера, функция возвращает PID созданного процесса
start()->
    Q = [],
    spawn(?MODULE,server,[Q, sets:new()]).
%% @doc запуск процесса очереди и процесса чтения rss по URL
start(Url) ->
    QPid = start(),
    rss_reader:start(Url,QPid),
    QPid.


%% @doc Функция Server - цикл сервера процесса очереди RSS
server(Q, Subs)->
    receive
        %% @doc Добавление нового элемента в очередь RSS
        {add_item, RSSItem} ->
            NewQ = add_item_to_q(RSSItem,Q, Subs),
            server(NewQ, Subs);
        %% @doc Получение всего содержимого очереди 
        {get_all, ReqPid} ->
            ReqPid ! {self(), Q},
            ?INFO("Sent rss items to ~p~n",[ReqPid]),
            server(Q, Subs);
        %% @doc Удаление pid из subscribers
        {unsubscribe,QPid} ->
            ?INFO("~p unsubscribed from ~p~n",[QPid,self()]),
            server(Q,sets:del_element(QPid,Subs));
        %% @doc Подписка очереди на ленту
        {subscribe,QPid} ->
            erlang:monitor(process,QPid),  % DOWN message if terminated
            ?INFO("New subscriber ~p to ~p~n",[QPid,self()]),
            [add_item(QPid,Item) || Item <- Q],
            server(Q,sets:add_element(QPid,Subs));
        %% @doc Отписка по сообщению DOWN
        {'DOWN',_,_,QPid,_Reason}->
            server(Q,sets:del_element(QPid,Subs));
        _Msg -> io:format("Unknown msg~p~n",[_Msg])  
    end.

%% @doc Рассылка обекта нескольким процессам
broadcast(Item, PidSet) ->
    [add_item(Pid, Item) || Pid <- sets:to_list(PidSet)].


%% @doc Отправка элемента в очередь
add_item(QPid, Item)->
    QPid ! {add_item,Item},
    ok.

%% @doc Извлечение элементов из ленты и отправка в очередь по порядку
add_feed(QPid, RSS2Feed) when is_pid(QPid) ->
    Items=rss_parse:get_feed_items(RSS2Feed),
    [add_item(QPid,Item) || Item <- Items],
    ?INFO("Added ~p feed items to queue with id: ~p ~n",[length(Items), QPid]),
    ok.

%% @doc Функция обновления элемента
add_item_to_q(NewItem,Q, Subs)->
    add_item_to_q_helper(NewItem,[],Q, Subs).

%% @doc Вспомогательная функция обновления элемента, когда прошли всю очередь
add_item_to_q_helper(NewItem,L1,[], Subs)->
    ?INFO("New found!!! ID: ~p ~n",[self()]),
    broadcast(NewItem, Subs),
    L1++[NewItem];
%% @doc Вспомогательная функция обновления элемента
%% Проходимся по элементам, и проверяем, новый ли item, или обновленный, или уже существующий
add_item_to_q_helper(NewItem,L1,L=[OldItem|Rest], Subs)->
    case rss_parse:compare_feed_items(OldItem,NewItem) of
        same -> 
            L1++L;
        updated -> 
            ?INFO("Update found!!! ~p ~n",[self()]),
            broadcast(NewItem, Subs),
            L1++Rest++[NewItem];
        different -> 
            add_item_to_q_helper(NewItem,L1++[OldItem],Rest, Subs)
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
