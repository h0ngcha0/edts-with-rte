%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc RTE resource
%%% @end
%%%
%%% This file is part of EDTS.
%%%
%%% EDTS is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% EDTS is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with EDTS. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_resource_rte).

%%%_* Exports ==================================================================

%% API
%% Webmachine callbacks
-export([ allowed_methods/2
        , allow_missing_post/2
        , content_types_accepted/2
        , content_types_provided/2
        , init/1
        , malformed_request/2
        , process_post/2
        , resource_exists/2]).

%% Handlers
-export([ from_json/2, to_json/2]).

%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================

%%%_* Types ====================================================================
%%%_* API ======================================================================

%% Webmachine callbacks
init(_Config) ->
  {ok, orddict:new()}.

allowed_methods(ReqData, Ctx) ->
  {['GET', 'POST'], ReqData, Ctx}.

allow_missing_post(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
  Map = [ {"application/json", from_json} ],
  {Map, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Map = [ {"application/json", to_json}
        , {"text/html",        to_json}
        , {"text/plain",       to_json}],
  {Map, ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
  %% edts_resource_lib:validate(ReqData, Ctx, [nodename, cmd, exclusions]).
  edts_resource_lib:validate(ReqData, Ctx, [nodename]).
  %% {false, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
  {edts_resource_lib:exists_p(ReqData, Ctx, [nodename]), ReqData, Ctx}.

%% Handlers
process_post(ReqData, Ctx) ->
  from_json(ReqData, Ctx).

from_json(ReqData, Ctx) ->
  Node        = orddict:fetch(nodename, Ctx),
  {Cmd, Args} = retrieve_cmd_and_args(ReqData),
  io:format( "in from_json. Node:~p Command:~p Args:~p~n"
           , [Node, Cmd, Args]),
  Info        = run_command(Cmd, Args, Node),
  io:format("command info:~p~n", [Info]),
  Data    = encode_rte_info(Info),
  {true, wrq:set_resp_body(mochijson2:encode(Data), ReqData), Ctx}.

to_atom(Bin) when is_list(Bin)->
  lists:map(fun to_atom/1, Bin);
to_atom(Bin) ->
  list_to_atom(binary_to_list(Bin)).

mk_convert_fun(rte_run)                    ->
  fun (Args) ->
      [Module, Fun, ArgumentsB] = Args,
      [to_atom(Module), to_atom(Fun), ArgumentsB]
  end;
mk_convert_fun(_)                          ->
  fun (Modules) ->
      lists:map(fun to_atom/1, Modules)
  end.

to_json(ReqData, Ctx) ->
  Node    = orddict:fetch(nodename, Ctx),
  Command = orddict:fetch(cmd, Ctx),
  io:format("cmd:~p~n", [Command]),
  Info    = edts:Command(Node),
  Data    = encode_rte_info(Info),
  {mochijson2:encode(Data), ReqData, Ctx}.

%%%_* Internal functions =======================================================
%%------------------------------------------------------------------------------
%% @doc
%% Encodes rte replies into the appropriate json structure
%% @end
-spec encode_rte_info({ok, Info :: term()}) -> term().
%%------------------------------------------------------------------------------
encode_rte_info({ok, Info}) ->
  do_encode_rte_info(Info);
encode_rte_info({error, Error}) ->
  [{state, error}, {message, Error}].

do_encode_rte_info(State) ->
  [{state, State}].

retrieve_cmd_and_args(ReqData) ->
  do_retrieve_cmd_and_args(
    mochijson2:decode(mochijson2:decode(wrq:req_body(ReqData)))).

do_retrieve_cmd_and_args({struct,[{<<"cmd">>, Cmd}, {<<"args">>, Args}]}) ->
  Command    = to_atom(Cmd),
  ConvertFun = mk_convert_fun(Command),
  {Command, ConvertFun(Args)};
do_retrieve_cmd_and_args({struct,[{<<"cmd">>, Cmd}]}) ->
  {to_atom(Cmd), nil}.

run_command(rte_run, [Module, Fun, Args], Node)                 ->
  rte_run(Node, Module, Fun, Args).

rte_run(Node, Module, Func, Args) ->
  case edts_dist:call(Node, edts_rte_server, rte_run, [Module, Func, Args]) of
    {badrpc, _} -> {error, not_found};
    Result      -> Result
  end.

%%%_* Unit tests ===============================================================
init_test() ->
  ?assertEqual({ok, orddict:new()}, init(foo)).

allowed_methods_test() ->
  ?assertEqual({['GET', 'POST'], foo, bar}, allowed_methods(foo, bar)).

content_types_accepted_test() ->
  ?assertEqual({[ {"application/json", from_json} ], foo, bar},
               content_types_accepted(foo, bar)).

content_types_provided_test() ->
  ?assertEqual({[ {"application/json", to_json}
                , {"text/html",        to_json}
                , {"text/plain",       to_json} ], foo, bar},
              content_types_provided(foo, bar)).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
