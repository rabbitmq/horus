-module(compatibility_test).

-export([generate/1,
         run/1]).

-record(my_record, { field }).

generate(Filename) ->
    {ok, _} = application:ensure_all_started(horus),

    Fun = fun() ->
                  %% Test maps.
                  Map1 = #{field => value1},
                  Map2 = Map1#{field => value2},
                  #{field := value2} = Map2,

                  %% Test records.
                  Record1 = #my_record{field = value1},
                  Record2 = Record1#my_record{field = value2},
                  true = (
                    is_tuple(Record2) andalso
                    element(#my_record.field, Record2) =:= value2),

                  %% Test case blocks.
                  case node() of
                      some_node -> throw(impossible);
                      _         -> ok
                  end,

                  %% Test message send/receive.
                  Pid = self(),
                  Pid ! message,
                  receive message -> ok after 1000 -> throw(receive_timeout) end,

                  file:delete(Filename),
                  io:format("Success!~n", [])
          end,
    StandaloneFun = horus:to_standalone_fun(Fun),

    Bin = list_to_binary(io_lib:format("~p.~n", [StandaloneFun])),
    ok = file:write_file(Filename, Bin),
    ok.

run(Filename) ->
    {ok, _} = application:ensure_all_started(horus),
    {ok, [StandaloneFun]} = file:consult(Filename),
    horus:exec(StandaloneFun, []).
