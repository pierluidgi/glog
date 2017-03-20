-module(glog).

-export([
    to_binary/1,
    msg/2, msg/3, msg/4, msg/5, msg/6, msg/7, msg/8,
    debug/2, debug/3, debug/4, debug/5, debug/6, debug/7,
    info/2, info/3, info/4, info/5, info/6, info/7,
    notice/2, notice/3, notice/4, notice/5, notice/6, notice/7,
    warn/2, warn/3, warn/4, warn/5, warn/6, warn/7,
    err/2, err/3, err/4, err/5, err/6, err/7,
    alert/2, alert/3, alert/4, alert/5, alert/6, alert/7,
    ex/3
]).

to_binary(Term) when is_binary(Term) ->
    Term;
to_binary(Term) when is_list(Term) ->
    try
        list_to_binary(Term)
    catch
        _:_ -> list_to_binary(io_lib:format("~p", [Term]))
    end;
to_binary(Term) when is_integer(Term) ->
    integer_to_binary(Term);
to_binary(Term) when is_float(Term) ->
    float_to_binary(Term, [{decimals, 10}, compact]);
to_binary(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
to_binary(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).

to_integer(Term) when is_integer(Term) ->
    Term;
to_integer(Term) when is_binary(Term) ->
    try
        binary_to_integer(Term)
    catch
        _ -> 0
    end;
to_integer(Term) when is_list(Term) ->
    try
        list_to_integer(Term)
    catch
        _ -> 0
    end;
to_integer(_) ->
    0.

severity_level(Level) ->
    case Level of
        emerg -> 0; alert -> 1;
        crit -> 2; err -> 3;
        warn -> 4; notice -> 5;
        info -> 6; debug -> 7;
        _ -> 1
    end.

create_msg(ShortMessage, FullMessage, File, Line, Level, Extra, Facility) ->
    Msg1 = #{
        <<"version">> => <<"1.1">>,
        <<"timestamp">> => erlang:system_time(millisecond) / 1000,
        <<"short_message">> => to_binary(ShortMessage)
    },
    
    Msg2 = case FullMessage of
        undefined ->
            Msg1;
        _ ->
            maps:put(<<"full_message">>, to_binary(FullMessage), Msg1)
    end,
    
    Msg3 = case Level of
        undefined ->
            Msg2;
        _ ->
            maps:put(<<"level">>, severity_level(Level), Msg2)
    end,
    
    Msg4 = case File of
        undefined ->
            Msg3;
        _ ->
            maps:put(<<"_file">>, to_binary(File), Msg3)
    end,
    
    Msg5 = case Line of
        undefined ->
            Msg4;
        _ ->
            maps:put(<<"_line">>, to_integer(Line), Msg4)
    end,
    
    Msg6 = case Facility of
        undefined ->
            Msg5;
        _ ->
            maps:put(<<"_facility">>, to_binary(Facility), Msg5)
    end,
    
    Msg7 = case Extra of
        undefined ->
            Msg6;
        _ ->
            ExtraMap = maps:from_list([{<<"_", (to_binary(Key))/binary>>, to_binary(Value)} || {Key, Value} <- maps:to_list(Extra)]),
            maps:merge(Msg6, ExtraMap)
    end,
    
    Msg7.

cast_msg(SrvRef, ShortMessage, FullMessage, File, Line, Level, Extra, Facility) ->
    Msg = create_msg(ShortMessage, FullMessage, File, Line, Level, Extra, Facility),
    gen_server:cast(SrvRef, {gelf_msg, Msg}).

msg(SrvRef, ShortMessage) ->
    cast_msg(SrvRef, ShortMessage, undefined, undefined, undefined, undefined, undefined, undefined).
msg(SrvRef, ShortMessage, FullMessage) ->
    cast_msg(SrvRef, ShortMessage, FullMessage, undefined, undefined, undefined, undefined, undefined).
msg(SrvRef, ShortMessage, FullMessage, File) ->
    cast_msg(SrvRef, ShortMessage, FullMessage, File, undefined, undefined, undefined, undefined).
msg(SrvRef, ShortMessage, FullMessage, File, Line) ->
    cast_msg(SrvRef, ShortMessage, FullMessage, File, Line, undefined, undefined, undefined).
msg(SrvRef, ShortMessage, FullMessage, File, Line, Level) ->
    cast_msg(SrvRef, ShortMessage, FullMessage, File, Line, Level, undefined, undefined).
msg(SrvRef, ShortMessage, FullMessage, File, Line, Level, Extra) ->
    cast_msg(SrvRef, ShortMessage, FullMessage, File, Line, Level, Extra, undefined).
msg(SrvRef, ShortMessage, FullMessage, File, Line, Level, Extra, Facility) ->
    cast_msg(SrvRef, ShortMessage, FullMessage, File, Line, Level, Extra, Facility).

format_stacktrace([], _Index, Msg) ->
    Msg;
format_stacktrace([{Module, Fun, Arity, Location}|Trace], Index, Msg) ->
    NewMsg = case Location of
        [{file, File}, {line,Line}] ->
            <<Msg/binary, "\n", "#", (glog:to_binary(Index))/binary, " ", (glog:to_binary(File))/binary, "(", (glog:to_binary(Line))/binary, "): ", (glog:to_binary(Module))/binary, ":", (glog:to_binary(Fun))/binary, "/", (glog:to_binary(Arity))/binary>>;
        _ ->
            <<Msg/binary, "\n", "#", (glog:to_binary(Index))/binary, " ", (glog:to_binary(Module))/binary, ":", (glog:to_binary(Fun))/binary, "/", (glog:to_binary(Arity))/binary>>
    end,
    format_stacktrace(Trace, Index + 1, NewMsg).

ex(SrvRef, Type, Ex) ->
    Trace = erlang:get_stacktrace(),
    ShortMessage = <<(glog:to_binary(Type))/binary, " ", (glog:to_binary(Ex))/binary, " ", (glog:to_binary(self()))/binary>>,
    FullMessage = format_stacktrace(Trace, 0, ShortMessage),
    err(SrvRef, ShortMessage, FullMessage, undefined, undefined, #{<<"stacktrace">> => Trace}).

debug(SrvRef, ShortMessage) ->
    msg(SrvRef, ShortMessage, undefined, undefined, undefined, debug).
debug(SrvRef, ShortMessage, FullMessage) ->
    msg(SrvRef, ShortMessage, FullMessage, undefined, undefined, debug).
debug(SrvRef, ShortMessage, FullMessage, File) ->
    msg(SrvRef, ShortMessage, FullMessage, File, undefined, debug).
debug(SrvRef, ShortMessage, FullMessage, File, Line) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, debug).
debug(SrvRef, ShortMessage, FullMessage, File, Line, Extra) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, debug, Extra).
debug(SrvRef, ShortMessage, FullMessage, File, Line, Extra, Facility) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, debug, Extra, Facility).

info(SrvRef, ShortMessage) ->
    msg(SrvRef, ShortMessage, undefined, undefined, undefined, info).
info(SrvRef, ShortMessage, FullMessage) ->
    msg(SrvRef, ShortMessage, FullMessage, undefined, undefined, info).
info(SrvRef, ShortMessage, FullMessage, File) ->
    msg(SrvRef, ShortMessage, FullMessage, File, undefined, info).
info(SrvRef, ShortMessage, FullMessage, File, Line) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, info).
info(SrvRef, ShortMessage, FullMessage, File, Line, Extra) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, info, Extra).
info(SrvRef, ShortMessage, FullMessage, File, Line, Extra, Facility) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, info, Extra, Facility).

notice(SrvRef, ShortMessage) ->
    msg(SrvRef, ShortMessage, undefined, undefined, undefined, notice).
notice(SrvRef, ShortMessage, FullMessage) ->
    msg(SrvRef, ShortMessage, FullMessage, undefined, undefined, notice).
notice(SrvRef, ShortMessage, FullMessage, File) ->
    msg(SrvRef, ShortMessage, FullMessage, File, undefined, notice).
notice(SrvRef, ShortMessage, FullMessage, File, Line) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, notice).
notice(SrvRef, ShortMessage, FullMessage, File, Line, Extra) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, notice, Extra).
notice(SrvRef, ShortMessage, FullMessage, File, Line, Extra, Facility) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, notice, Extra, Facility).

warn(SrvRef, ShortMessage) ->
    msg(SrvRef, ShortMessage, undefined, undefined, undefined, warn).
warn(SrvRef, ShortMessage, FullMessage) ->
    msg(SrvRef, ShortMessage, FullMessage, undefined, undefined, warn).
warn(SrvRef, ShortMessage, FullMessage, File) ->
    msg(SrvRef, ShortMessage, FullMessage, File, undefined, warn).
warn(SrvRef, ShortMessage, FullMessage, File, Line) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, warn).
warn(SrvRef, ShortMessage, FullMessage, File, Line, Extra) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, warn, Extra).
warn(SrvRef, ShortMessage, FullMessage, File, Line, Extra, Facility) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, warn, Extra, Facility).

err(SrvRef, ShortMessage) ->
    msg(SrvRef, ShortMessage, undefined, undefined, undefined, err).
err(SrvRef, ShortMessage, FullMessage) ->
    msg(SrvRef, ShortMessage, FullMessage, undefined, undefined, err).
err(SrvRef, ShortMessage, FullMessage, File) ->
    msg(SrvRef, ShortMessage, FullMessage, File, undefined, err).
err(SrvRef, ShortMessage, FullMessage, File, Line) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, err).
err(SrvRef, ShortMessage, FullMessage, File, Line, Extra) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, err, Extra).
err(SrvRef, ShortMessage, FullMessage, File, Line, Extra, Facility) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, err, Extra, Facility).

alert(SrvRef, ShortMessage) ->
    msg(SrvRef, ShortMessage, undefined, undefined, undefined, alert).
alert(SrvRef, ShortMessage, FullMessage) ->
    msg(SrvRef, ShortMessage, FullMessage, undefined, undefined, alert).
alert(SrvRef, ShortMessage, FullMessage, File) ->
    msg(SrvRef, ShortMessage, FullMessage, File, undefined, alert).
alert(SrvRef, ShortMessage, FullMessage, File, Line) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, alert).
alert(SrvRef, ShortMessage, FullMessage, File, Line, Extra) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, alert, Extra).
alert(SrvRef, ShortMessage, FullMessage, File, Line, Extra, Facility) ->
    msg(SrvRef, ShortMessage, FullMessage, File, Line, alert, Extra, Facility).