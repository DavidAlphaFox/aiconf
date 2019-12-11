%%%-------------------------------------------------------------------
%%% @author David Gao <david@Davids-MacBook-Pro.local>
%%% @copyright (C) 2019, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 14 Feb 2019 by David Gao <david@Davids-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(ai_conf_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([load_conf/2,sections/1,section/3,value/4]).


-define(SERVER, ?MODULE).
-define(TAB,ai_conf).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
load_conf(ConfName,Files) ->
    gen_server:call(?MODULE, {load_conf,{ConfName,Files}}, infinity).

sections(ConfName) ->
    Matches = ets:match(?TAB, {{ConfName, '$1', '_'}, '_'}),
    lists:umerge(Matches).

section(ConfName,SectionKey,Default) ->
    SectionKey0 = ai_string:to_string(SectionKey),
    Matches = ets:match(?TAB, {{ConfName, SectionKey0, '$1'}, '$2'}),
    case Matches of 
        [] -> Default;
        _-> [{Key, Value} || [Key, Value] <- Matches]
    end.
value(ConfName, SectionKey, Key, Default) ->
    SectionKey0 = ai_string:to_string(SectionKey),
    Key0 = ai_string:to_string(Key),
    case ets:lookup(?TAB, {ConfName, SectionKey0, Key0}) of
        [] -> Default;
        [{_, Match}] -> Match
    end.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
    init_ets(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init([]) ->
    process_flag(trap_exit, true),
    load_conf(),
    {ok, #state{}}.

%%----------------------------------------------------1----------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({load_conf,Msg},_From,State)->
		Reply = load_conf([Msg]),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_ets() ->
    case ets:info(?TAB, name) of 
        undefined ->
            ets:new(?TAB, [ordered_set, public, named_table,
                           {read_concurrency, true},
                           {write_concurrency, true}]);
        _ -> true
    end.
load_conf()->
    case application:get_env(econfig, app_conf) of
        undefined -> ok;
        {ok, {Confs}} -> load_conf(Confs)
    end.
load_conf([])-> ok;
load_conf([{ConfName, ConfFiles} | Rest])->
    lists:foreach(fun(ConfFile) ->
                      Parsed = parse(ConfName,ConfFile),
                      ets:insert(?TAB,Parsed)
                  end, ConfFiles),
    load_conf(Rest).

parse(ConfName, ConfFile)->
    Json = 
        case file:read_file(ConfFile) of
            {ok,Bin} -> Bin;
            {error, eacces} ->throw({file_permission_error,ConfFile});
            {error, enoent} ->
                Fmt = "Couldn't find  configuration file ~s.",
                Msg = list_to_binary(io_lib:format(Fmt, [ConfFile])),
                throw({startup_error, Msg})
        end,
    Decoded = jiffy:decode(Json),
    lists:foldl(fun({SectionKey,SectionData},Acc)->
                       lists:foldl(fun({Key,Value},Acc1)-> 
                                           [{{ConfName,SectionKey,Key},Value}|Acc1]
                                   end,Acc,SectionData)
               end,[],Decoded).
