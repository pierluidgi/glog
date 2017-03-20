-module(glog_handler).

-behaviour(gen_event).

-export([
    init/1,
    handle_call/2,
    handle_info/2,
    handle_event/2,
    terminate/2,
    code_change/3
]).

init(SrvRef) ->
    {ok, #{srv_ref => SrvRef}}.

handle_call(_Request, State) ->
    {ok, null, State}.

handle_info(_Info, State) ->
    {ok, State}.

handle_event({error, _Gleader, {Pid, Format, Data}}, State) ->
    ShortMessage = <<"error ", (glog:to_binary(Pid))/binary>>,
    FullMessage = io_lib:format(Format, Data),
    glog:err(maps:get(srv_ref, State), ShortMessage, FullMessage),
    {ok, State};
    
handle_event({error_report, _Gleader, {Pid, std_error, Report}}, State) ->
    ShortMessage = <<"std_error ", (glog:to_binary(Pid))/binary>>,
    FullMessage = Report,
    glog:err(maps:get(srv_ref, State), ShortMessage, FullMessage),
    {ok, State};
    
handle_event({error_report, _Gleader, {Pid, Type, Report}}, State) ->
    ShortMessage = <<(glog:to_binary(Type))/binary, " ", (glog:to_binary(Pid))/binary>>,
    FullMessage = Report,
    glog:err(maps:get(srv_ref, State), ShortMessage, FullMessage),
    {ok, State};
    
handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State) ->
    ShortMessage = <<"warning_msg ", (glog:to_binary(Pid))/binary>>,
    FullMessage = io_lib:format(Format, Data),
    glog:warn(maps:get(srv_ref, State), ShortMessage, FullMessage),
    {ok, State};
    
handle_event({warning_report, _Gleader, {Pid, std_warning, Report}}, State) ->
    ShortMessage = <<"std_warning ", (glog:to_binary(Pid))/binary>>,
    FullMessage = Report,
    glog:warn(maps:get(srv_ref, State), ShortMessage, FullMessage),
    {ok, State};
    
handle_event({warning_report, _Gleader, {Pid, Type, Report}}, State) ->
    ShortMessage = <<(glog:to_binary(Type))/binary, " ", (glog:to_binary(Pid))/binary>>,
    FullMessage = Report,
    glog:warn(maps:get(srv_ref, State), ShortMessage, FullMessage),
    {ok, State};
    
handle_event({info_msg, _Gleader, {_Pid, _Format, _Data}}, State) ->
    %ShortMessage = <<"info_msg ", (glog:to_binary(Pid))/binary>>,
    %FullMessage = io_lib:format(Format, Data),
    %glog:info(maps:get(srv_ref, State), ShortMessage, FullMessage),
    {ok, State};
    
handle_event({info_report, _Gleader, {_Pid, std_info, _Report}}, State) ->
    %ShortMessage = <<"std_info ", (glog:to_binary(Pid))/binary>>,
    %FullMessage = Report,
    %glog:info(maps:get(srv_ref, State), ShortMessage, FullMessage),
    {ok, State};
    
handle_event({info_report, _Gleader, {_Pid, _Type, _Report}}, State) ->
    %ShortMessage = <<(glog:to_binary(Type))/binary, " ", (glog:to_binary(Pid))/binary>>,
    %FullMessage = Report,
    %glog:info(maps:get(srv_ref, State), ShortMessage, FullMessage),
    {ok, State};
    
handle_event(_Event, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.