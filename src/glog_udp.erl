-module(glog_udp).

-behaviour(gen_server).

-export([
    start_link/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(UDP_CHUNK_SIZE, 1432).
-define(GELF_CHUNK_SIZE, 1420).
-define(GELF_MAX_CHUNKS, 128).

-define(QUEUE_LIMIT, 500).

start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).

init(Args) ->
    IpVersion = case maps:get(<<"ip_version">>, Args, undefined) of
        inet -> inet;
        ipv4 -> inet;
        inet6 -> inet6;
        ipv6 -> inet6;
        _ -> inet
    end,
    
    {ok, Hostname} = inet:gethostname(),
    
    {ok, Socket} = gen_udp:open(0, [IpVersion, {mode, binary}, {active, false}]),
    
    State = #{
        <<"mon_ref">> => undefined,
        <<"queue">> => queue:new(),
        <<"host">> => glog:to_binary(Hostname),
        <<"node">> => glog:to_binary(node()),
        <<"facility">> => glog:to_binary(maps:get(<<"facility">>, Args, <<"undefined">>)),
        <<"socket">> => Socket,
        <<"dest_host">> => maps:get(<<"host">>, Args),
        <<"dest_port">> => maps:get(<<"port">>, Args)
    },
    
    {ok, State}.

ceil(X) ->
	T = erlang:trunc(X),
    
	case (X - T) of
		Neg when Neg < 0 -> T;
		Pos when Pos > 0 -> T + 1;
		_ -> T
    end.

make_chunk(ChunkData, MessageId, ChunkNum, ChunksCount) ->
	BinChunkNum =  binary:encode_unsigned(ChunkNum),
    BinChunksCount =  binary:encode_unsigned(ChunksCount),
    <<16#1e, 16#0f, MessageId:8/binary, BinChunkNum:1/binary, BinChunksCount:1/binary, ChunkData/binary>>.

send_chunked_msg(Socket, Host, Port, Data, MessageId, ChunkNum, ChunksCount) ->
    case byte_size(Data) > ?GELF_CHUNK_SIZE of
        true ->
            <<ChunkData:?GELF_CHUNK_SIZE/binary, RestData/binary>> = Data,
            gen_udp:send(Socket, Host, Port, make_chunk(ChunkData, MessageId, ChunkNum, ChunksCount)),
            send_chunked_msg(Socket, Host, Port, RestData, MessageId, ChunkNum + 1, ChunksCount);
        false ->
            gen_udp:send(Socket, Host, Port, make_chunk(Data, MessageId, ChunkNum, ChunksCount))
    end.

compress_msg(Msg) ->
    Stream = zlib:open(),
    zlib:deflateInit(Stream, 9),
    Compressed = zlib:deflate(Stream, Msg, finish),
    zlib:deflateEnd(Stream),
    list_to_binary(Compressed).

prepare_msg(Msg, Host, Node, Facility) ->
    FullMsg = maps:merge(#{
        <<"host">> => Host,
        <<"_node">> => Node,
        <<"_facility">> => Facility
    }, Msg),
    
    compress_msg(jsx:encode(FullMsg)).

spawn_send_msg(Data, Socket, Host, Port) ->
    spawn_monitor(fun() ->
        try
            Size = byte_size(Data),
            
            if
                Size > ?UDP_CHUNK_SIZE ->
                    ChunksCount = ceil(Size / ?GELF_CHUNK_SIZE),
                    
                    if
                        ChunksCount =< ?GELF_MAX_CHUNKS ->
                            MessageId = list_to_binary([rand:uniform(255) || _ <- lists:seq(1, 8)]),
                            send_chunked_msg(Socket, Host, Port, Data, MessageId, 0, ChunksCount);
                        true ->
                            do_nothing
                    end;
                true ->
                    gen_udp:send(Socket, Host, Port, Data)
            end
        catch
            _:_ -> do_nothing
        end
    end).

process_msg(Msg, #{<<"mon_ref">> := MonRef, <<"queue">> := Queue} = State) ->
    Len = queue:len(Queue),
    
    if
        Len < ?QUEUE_LIMIT ->
            Data = try
                prepare_msg(Msg, maps:get(<<"host">>, State), maps:get(<<"node">>, State), maps:get(<<"facility">>, State))
            catch
                _:_ -> undefined
            end,
            
            case Data of
                undefined ->
                    State;
                _ ->
                    case MonRef of
                        undefined ->
                            Ref = spawn_send_msg(Data, maps:get(<<"socket">>, State), maps:get(<<"dest_host">>, State), maps:get(<<"dest_port">>, State)),
                            State#{<<"mon_ref">> => Ref};
                        _ ->
                            State#{<<"queue">> => queue:in(Data, Queue)}
                    end
            end;
        true ->
            State
    end.

handle_call({gelf_msg, Msg}, _From, State) ->
    {reply, ok, process_msg(Msg, State)};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({gelf_msg, Msg}, State) ->
    {noreply, process_msg(Msg, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, #{<<"queue">> := Queue} = State) ->
    erlang:demonitor(MonitorRef),
    
    {Result, NewQueue} = queue:out(Queue),
    
    NewState = case Result of
        {value, Data} ->
            NewMonRef = spawn_send_msg(Data, maps:get(<<"socket">>, State), maps:get(<<"dest_host">>, State), maps:get(<<"dest_port">>, State)),
            State#{<<"mon_ref">> => NewMonRef, <<"queue">> => NewQueue};
        _ ->
            State#{<<"mon_ref">> => undefined}
    end,
    
    {noreply, NewState};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    gen_udp:close(maps:get(<<"socket">>, State)).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.