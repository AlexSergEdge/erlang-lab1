-module(rss_queue).
-compile(export_all).
-define(TIMEOUT,10000).
-include("logging.hrl").
% -import(rss_parse, [get_feed_items/1, compare_feed_items/2]).
-include_lib("xmerl/include/xmerl.hrl").

% The server implements the gen_server behavior.
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% Additional helper functions exported by the callback module.
-export([start/1]).

-record(rssQ,{queue,subscribers}).

% TODO:  Implement module helper functions.
% start(Name) -> gen_server:start({local, Name}, ?MODULE, [], []).
% TODO:  Behavior callback functions.  Implement the appropriate callback
%        functions as necessary for your server.
% init([]) -> {ok, todo_state}.
start(Name) -> 
  gen_server:start({local, Name}, ?MODULE, [], []).
start(Name,Url)->
  gen_server:start({local, Name}, ?MODULE, [Url], []).

%% @doc Инициализация (при вызове gen_server:start):
init([]) ->
    process_flag(trap_exit,true),
    {ok, #rssQ{queue=[],subscribers=sets:new()} };
init([Url]) -> 
    State = #rssQ{queue=[],subscribers=sets:new()},
    process_flag(trap_exit,true),
    rss_reader:start(Url,self()),
    {ok, State }.

%% @doc Вызывается с помощью gen_server:call
%% @doc - запрос на подписку
handle_call(_Request = {subscribe,QPid}, _From, State=#rssQ{queue=Q,subscribers=Subs}) ->
    % Проверяем что pid среди подписчиков, если нет, то подписываем
    {Reply,NewState} = case sets:is_element(QPid,Subs) of
        true -> {{error,already_subscribed},State};
        false ->  erlang:monitor(process,QPid),
        ?INFO("New subscriber ~p to ~p~n",[QPid,self()]),
        [add_item(QPid,Item) || Item <- Q],
        {ok, State#rssQ{subscribers=sets:add_element(QPid,Subs)}}
    end,
    {reply, Reply, NewState};
%% @doc - запрос на отображение всех - возвращаем список
handle_call(_Request={get_all}, _From, State=#rssQ{queue=Q}) -> 
    {reply,Q,State};
%% @doc - запрос неизвестен
handle_call(_Request, _From, State) -> 
    {reply,{error,{unknown_request,_Request}}, State}.



%% @doc Вызывается при вызове gen_server:cast
%% @doc Добавление элемента в очередь
handle_cast(_Msg={add_item,RSSItem=#xmlElement{name=item}}, State=#rssQ{queue=Q,subscribers=Subs}) -> 
    NewQ = add_item_to_q(RSSItem,Q,Subs),
    {noreply,State#rssQ{queue=NewQ}};
%% @doc Отписка от очереди
handle_cast(_Msg={unsubscribe,QPid}, State=#rssQ{subscribers=Subs}) -> 
    {noreply,State#rssQ{subscribers=sets:del_element(QPid,Subs)}};
%% @doc Неизвестное сообщение
handle_cast(_Msg, State) -> 
    ?WARN("Unknown msg {~p} to Q{~p}",[_Msg,State]),
    {noreply, State}.


%% @doc Эта функция вызывается когда происходит таймаут ожидания
handle_info(_Info={'DOWN',_,_,QPid,_Reason},State=#rssQ{subscribers=Subs})->
    {noreply, State#rssQ{subscribers=sets:del_element(QPid,Subs)}};
handle_info(_Info={'EXIT',FromPid,_Reason},State)->
    ?ERROR("RSS Reader ~p died for ~p with reason ~n",[FromPid,self(),_Reason]),
    {noreply, State};
handle_info(_Info, State) -> 
    {noreply, State}.


%% @doc Эта функция вызывается перед завершением gen_server процесса 
terminate(_Reason, _State) -> ok.


%% @doc Эта функция вызывается когда происходит обновление/откат версии gen_server процесса и ему необходимо обновить свое внутреннее состояние
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @doc Подписать очередь на сообщения другой очереди
subscribe(From,To)->
  gen_server:call(To,{subscribe,From}).

%% @doc Рассылка обекта нескольким процессам
broadcast(Item, PidSet) ->
    [add_item(Pid, Item) || Pid <- sets:to_list(PidSet)].

%% @doc Отправка элемента в очередь
add_item(QPid, Item)->
    ok = gen_server:cast(QPid , {add_item,Item} ),
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
    gen_server:call(QPid,{get_all}).
