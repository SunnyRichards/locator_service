%%%-------------------------------------------------------------------
%% @doc locator_service public API
%% @end
%%%-------------------------------------------------------------------

-module(locator_service_app).

-behaviour(application).

%% Application callbacks

-record(cb_reference,  {org_name,token,node_name}).
-record(chb_reference, {org_name,token,node_name}).

-export([start/2, stop/1]).
-export([do_it_once/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, StartArgs) ->
  io:format("Start: ~p~n",[StartArgs]),
  application:start(worker_pool),
  case proplists:get_value(config_file,StartArgs) of
    FileLocation when is_list(FileLocation) ->
      try (yamerl:decode_file(FileLocation)) of
        [Config] when is_list(Config) ->
          io:format("File Decoded : ~p~n",[Config]),
          Service = list_to_atom(proplists:get_value("service_name",Config)),
          io:format("Service: ~p~n",[Service]),
          Arguments = make_arguments(Config),
          io:format("Arguments: ~p~n",[Arguments]),
          case supervisor:start_child(worker_pool_service_sup,[Service,Arguments]) of
            {ok, Pid} -> {ok, Pid};
            Error -> Error
          end;
        _->{error,<<"Yaml Parser Error">>}
      catch
        _:Reason -> {error,Reason}
      end;
    _ ->{error,<<"undefined yamel file location">>}
  end.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

do_it_once() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_tables().

create_tables() ->
    mnesia:create_table(cb_reference,[{disc_copies, [node()]},{index,[#cb_reference.node_name]},{attributes, record_info(fields,cb_reference)}]),
    mnesia:create_table(chb_reference,[{disc_copies, [node()]},{index,[#chb_reference.node_name]},{attributes, record_info(fields,chb_reference)}]).

make_arguments(Config) ->
[
 {max_request_in_queue, proplists:get_value("max_request_in_queue",Config)},
 {allowed_workers,proplists:get_value("allowed_workers",Config)},
 {queue_range,proplists:get_value("queue_range",Config)},
  {sub,[
        {low,proplists:get_value("low",Config),{list_to_atom(proplists:get_value("behaviour",Config)),[],[{transaction_weight,120}]}},
        {medium,proplists:get_value("medium",Config),{list_to_atom(proplists:get_value("behaviour",Config)),[],[{transaction_weight,120}]}},
        {high,proplists:get_value("high",Config),{list_to_atom(proplists:get_value("behaviour",Config)),[],[{transaction_weight,120}]}}
       ]}
 ].