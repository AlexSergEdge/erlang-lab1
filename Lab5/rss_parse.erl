-module(rss_parse).

-export([is_rss2_feed/1, compare_feed_items/2]).
-export([get_feed_items_from_file/1, get_item_time_from_file/1, compare_RSS2_from_file_diff/1, compare_RSS2_from_file_same/1]).
-export([get_feed_items/1]).



-include_lib("xmerl/include/xmerl.hrl").
-import(xmerl_xpath, [string/2]).
-import(lists, [map/2]).

% Task 2.
% Check if it is RSS 2.0 format
% returns True if <rss version="2.0"> is present
%   rss_parse:is_rss2_feed(example1.xml).  (returns True)
%   rss_parse:is_rss2_feed(example1_not_rss2.xml).  (returns False)
is_rss2_feed(XML) ->
    case XML#xmlElement.name of
        rss -> 
            case (lists:nth(1, xmerl_xpath:string("@version", XML)))#xmlAttribute.value of
                "2.0" -> ok;
                _ -> false
            end; 
        _ -> false
    end.

% Task 3.
% Get list of <item> element's values (<item>...</item>)
get_feed_items(RSS2Feed) ->
    xmerl_xpath:string("//channel/item", RSS2Feed).


% Task 4.
% Get time from item
get_item_time(Item) ->
    [Time] = xmerl_xpath:string("//pubDate/text()", Item),
    DateTime = httpd_util:convert_request_date(Time#xmlText.value),
    calendar:datetime_to_gregorian_seconds(DateTime).




% Task 5.
% compare two elements of RSS feed
compare_feed_items(ItemP1, ItemP2) ->
	Item1 = extract_xml(ItemP1),
	Item2 = extract_xml(ItemP2),
	Guid1 = get_guid(Item1),
	Guid2 = get_guid(Item2),
	Title1 = get_title(Item1),
	Title2 = get_title(Item2),
	Link1 = get_link(Item1),
	Link2 = get_link(Item2),
    compare_RSS2_feed_items(Guid1, Title1, Link1, Guid2, Title2, Link2).

% @private
% @doc Эта вспомогательная функция просматривает заданный XML элемент
%      и удаляет из него сведения о других XML элементах, например содержащиеся в полях
%      "parents" или "pos".
%
% @spec extract_xml(Node::xmlAny()) -> xmlAny()
%s
extract_xml(Elem = #xmlElement{}) ->
    Elem#xmlElement{parents=[], pos=0,
        content=lists:map(fun extract_xml/1, Elem#xmlElement.content),
        attributes=lists:map(fun extract_xml/1, Elem#xmlElement.attributes)};
extract_xml(Attr = #xmlAttribute{}) ->
    Attr#xmlAttribute{parents=[], pos=0};
extract_xml(Text = #xmlText{}) ->
    Text#xmlText{parents=[], pos=0};
extract_xml(Comment = #xmlComment{}) ->
    Comment#xmlComment{parents=[], pos=0};
extract_xml(Other) ->
    Other.


get_link(Item) -> 
    xmerl_xpath:string("link/text()", Item).
get_title(Item) -> 
    xmerl_xpath:string("title/text()", Item).
get_guid(Item) -> 
	xmerl_xpath:string("guid/text()", Item).


compare_RSS2_feed_items(Guid, Title, Link, Guid, Title, Link) ->
    same;
compare_RSS2_feed_items(Guid, _, _, Guid, _, _) ->
    updated;
compare_RSS2_feed_items(_, Title, _, _, Title, _) ->
    updated;
compare_RSS2_feed_items(_, _, Link, _, _, Link) ->
    updated;
compare_RSS2_feed_items(_, _, _, _, _, _) ->
    different.

% Get from file
% rss_parse:get_feed_items_from_file(example1.xml).
get_feed_items_from_file(File) ->
    {XML, _} = xmerl_scan:file(File),
    get_feed_items(XML).

% Get all times
% rss_parse:get_feed_items_from_file(example1.xml).
get_item_time_from_file(File) -> 
    {XML, _} = xmerl_scan:file(File),
    map(fun get_item_time/1, get_feed_items(XML)).


% compare diffrent or updated elems
% diff:  rss_parse:compare_RSS2_from_file_diff(example1.xml).
% ipd:   rss_parse:compare_RSS2_from_file_diff(example1_updated.xml).
compare_RSS2_from_file_diff(File) ->
    {XML, _} = xmerl_scan:file(File),
    [X1, X2| _] = get_feed_items(XML),
    compare_feed_items(X1, X2).

% compare same elems
% same:  rss_parse:compare_RSS2_from_file_same(example1.xml).
compare_RSS2_from_file_same(File) ->
    {XML, _} = xmerl_scan:file(File),
    [X1| _] = get_feed_items(XML),
    compare_feed_items(X1, X1).

