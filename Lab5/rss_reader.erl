-module(rss_reader).
-include("logging.hrl").
-compile(export_all).
-define(RETRIEVE_INTERVAL,200000).


%% @doc Запуск нового процесса
start(Url,QPid)->
    inets:start(),
    spawn(?MODULE, server, [Url,QPid]).

%% @doc Цикл процесса
server(Url, QPid)->
    %% @doc Загрузка ленты по URL
    {ok, {{_,Code,_},_,Load}} = httpc:request(Url),
    %% @doc Код ответа равен 200 => парсим XML
    case Code of 
        200 ->
            {Feed,_} = xmerl_scan:string(Load),
            %% @doc проверяем что на входе у нас все в формате RSS 2.0.
            case rss_parse:is_rss2_feed(Feed) of
            ok -> 
                %% @doc отправляем все элементы ленты в очередь
                rss_queue:add_feed(QPid,Feed),
                receive
                %% @doc Ждем заданный интервал
                after ?RETRIEVE_INTERVAL -> 
                    server(Url,QPid)
                end;
            %% @doc Не RSS2
            _ -> {error,not_rss2_feed}
            end ; 
        %% @doc Ошибка, если не ok
        _ -> {error,Code}
    end.
