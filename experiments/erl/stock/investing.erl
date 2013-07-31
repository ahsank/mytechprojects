-module(investing).
-export([get_historical_price/3]).
-export([generate_history/3]).

-record(day_price, {day, open, high, low, close, volume, adjclose}).
%-record(portfolio, {investments, investmentValue, cash}).
%-record(investment, {symbol, amount}).

url(Symbol) ->
      "http://finance.yahoo.com/d/quotes.csv?s=" ++ Symbol ++ "&f=l1".
 
	      
quote(Symbol) ->
    inets:start(),
    {ok,{_Status, _Headers, Response}} = httpc:request(get, {url(Symbol), []},
						      [{timeout, 5000}], [{sync, true}]),
    %erlang:display(Response),
    Values = re:split(Response, "[,\r\n]"),
    %erlang:display(Values),
    Values.

history_url(Symbol,Start, End) ->
    {SY, SM, SD} = Start,
    {EY, EM, ED} = End,
    lists:flatten(io_lib:format("http://ichart.finance.yahoo.com/table.csv?s=~s" 
				++ "&a=~B&b=~B&c=~B&d=~B&e=~B&f=~B&g=d",
				[Symbol, SM, SD, SY, EM, ED, EY])).


parse_history([]) ->
    [];
parse_history([H|T]) ->
    List = string:tokens(binary_to_list(H), ","),
    case List of 
	[] ->
	    parse_history(T);
	[Date, Open, High, Low, Close, Volume, AdjClose] ->
	    [Year, Month, Day] = string:tokens(Date, "-"),
	    [#day_price{day={list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)},
	     open=list_to_float(Open), high=list_to_float(High), low=list_to_float(Low), 
	     close=list_to_float(Close), volume=list_to_integer(Volume), 
			adjclose=list_to_float(AdjClose)}|
	     parse_history(T)]
    end.


get_historical_price(Symbol, Start, End) ->
    inets:start(),
    Url = history_url(Symbol, Start, End),
    erlang:display(Url),
    {ok,{_Status, _Headers, Response}} = httpc:request(get, {Url, []},
						      [{timeout, 5000}], [{sync, true}]),
    [_|Lines] = re:split(Response, "[\n]"),
    Lines1 = parse_history(Lines),
    %erlang:display(Lines),
    Lines1.


write_history(Handle, Symbol) ->
    H = get_historical_price(Symbol, {2013, 1, 1}, date()),
    io:format(Handle, "~p.~n", [H]).

% DayStream (daystream, [{tradingday, date, [{symbol, high, low, ...}]}
% ChartHistory {chartHistory, {symbol{date, high, low...}}

merge_history(Accum, [], _Symbol , []) ->
    Accum;

merge_history(Accum, [DayHistory|DHT], Symbol, []) ->
    merge_history([DayHistory|Accum], DHT, Symbol, []);

merge_history(Accum, [], Symbol, [DayPrice|DPT])  ->
    #day_price{day=Day} = DayPrice,
    merge_history([{tradingday,Day, [{Symbol, DayPrice}]} | Accum], [], Symbol, DPT);
  
merge_history(Accum, [DayHistory|DHT], Symbol, [DayPrice|DPT]) ->
    #day_price{day=Day} = DayPrice,
    {tradingday, TradingDate, SymbolPrices} = DayHistory,
    if TradingDate =:= Day ->
	    NewElem = {tradingday, TradingDate, [{Symbol, DayPrice}|SymbolPrices]},
	    merge_history([NewElem|Accum], DHT, Symbol, DPT);
       TradingDate > Day ->
	    merge_history([DayHistory|Accum], DHT, Symbol, [DayPrice|DPT]);
       TradingDate < Day ->
	    NewElem = {tradingday, Day, [{Symbol, DayPrice}]},
	    merge_history([NewElem|Accum], [DayHistory|DHT], Symbol, DPT)
    end.

accumulate_history(_File, DayStream, []) ->
    DayStream;

accumulate_history(File, DayStream, [Symbol|L]) ->
    Prices = get_historical_price(Symbol, {2013, 1, 1}, date()),
    io:format(File, "~p.~n", [{price_history, Symbol, Prices}]),
    NewStream = lists:reverse(merge_history([], DayStream, Symbol,  Prices)),
    accumulate_history(File, NewStream, L).

%
% example:  investing:generate_history("watchlist.txt", "chart.txt", "dayevent.txt").
%
generate_history(InputFile, OutputFile, StreamFile) ->
    {ok, L} = file:consult(InputFile),
    {ok, Handle} = file:open(OutputFile, write),
    DayHistory = lists:reverse(accumulate_history(Handle, [], L)),
    file:close(Handle),
    {ok, H1} = file:open(StreamFile, write),
    lists:map(fun(X) -> io:format(H1, "~p.~n", [X]) end, DayHistory),
    file:close(H1).

apply_strategy(PortFolio, _, _, []) ->
    PortFolio;

apply_strategy(PortFolio, StrategyFn, State, [DayPrice|DHT]) ->
    {NewPortFolio, NewState} = StrategyFn(PortFolio, State, DayPrice),
    apply_strategy(NewPortFolio, StrategyFn, NewState, DHT).

apply_strategy(StrategyFn, StreamFile) ->
    {ok, L} = file:consult(StreamFile),
    apply_strategy({portfolio, 100000.00, []}, StrategyFn, [], L).


% Stream of stock data
% going through the rule
% Buy and sell of stocks
% Current price of the portfolio

