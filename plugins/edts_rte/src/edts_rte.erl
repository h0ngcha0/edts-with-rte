%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
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

%% This module is the starting point of edts_rte. Its sole purpose is to
%% start the edts_rte supervisor.

%%%_* Module declaration =======================================================
-module(edts_rte).

-behaviour(edts_plugins).

%%%_* Exports =================================================================
-export([ forget_record_defs/1
        , interpret_module/1
        , list_record_names/0
        , rte_run/3
        , uninterpret_module/1
        , update_record_defs/1
        ]).

%% Behaviour exports
-export([edts_server_services/0,
         event_formatters/0,
         project_node_modules/0,
         project_node_services/0,
         spec/2]).

%%%_* Defines ==================================================================
%%%_* API ======================================================================
rte_run(Module, Func, Args) ->
  edts_rte_app:debug("rte_run args: ~p~n", [Args]),
  edts_rte_server:rte_run(Module, Func, Args).

interpret_module(Module) ->
  case module_interpreted_p(Module) of
    true  -> {ok, already_interpreted};
    false -> do_interpret_module(Module, true)
  end.

uninterpret_module(Module) ->
  case module_interpreted_p(Module) of
    true  -> do_interpret_module(Module, false);
    false -> {ok, already_uninterpreted}
  end.

list_record_names() ->
  edts_rte_server:list_record_names().

update_record_defs(Module) ->
  edts_rte_server:update_record_defs(Module).

forget_record_defs(RecordName) ->
  edts_rte_server:forget_record_defs(RecordName).

%% Behaviour callbacks
edts_server_services()  -> [].
event_formatters()      -> [].
project_node_modules()  ->
  [ edts_rte
  , edts_rte_app
  , edts_rte_util
  , edts_rte_int_listener
  , edts_rte_records
  , edts_rte_server
  , edts_rte_sup
  ].
project_node_services() -> [edts_rte_app].

spec(interpret_module,      1) -> [ {module,    atom}];
spec(uninterpret_module,    1) -> [ {module,    atom}];
spec(list_record_names,     0) -> [];
spec(update_record_defs,    1) -> [ {module,    atom}];
spec(forget_record_defs,    1) -> [ {record,    atom}];
spec(rte_run,               3) -> [ {module,    atom}
                                  , {function,  atom}
                                  , {args,      string}
                                  ].

%%%_* Internal =================================================================
do_interpret_module(Module, true) ->
  case module_interpretable_p(Module) of
    false -> {error, uninterpretable};
    true  ->
      {module, Module} = int:i(Module),
      true
  end;
do_interpret_module(Module, false) ->
  ok = int:n(Module),
  false.

%%------------------------------------------------------------------------------
%% @doc
%% Return true if Module is interpretable, false otherwise
%% @end
-spec module_interpretable_p(module()) -> boolean().
%%------------------------------------------------------------------------------
module_interpretable_p(Module) ->
  ensure_started(),
  case int:interpretable(Module) of
    true       -> true;
    {error, _} -> false
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Reports if Module is interpreted.
%% @end
-spec module_interpreted_p(Module :: module()) -> boolean().
%%------------------------------------------------------------------------------
module_interpreted_p(Module) ->
  ensure_started(),
  lists:member(Module, interpreted_modules()).

%%------------------------------------------------------------------------------
%% @doc
%% Return a list of all interpreted modules.
%% @end
-spec interpreted_modules() -> [module()].
%%------------------------------------------------------------------------------
interpreted_modules() ->
  ensure_started(),
  int:interpreted().

%%------------------------------------------------------------------------------
%% @doc
%% Ensure that the debug-server is running
%% @end
-spec ensure_started() -> ok.
%%------------------------------------------------------------------------------
ensure_started() ->
  %% TODO: ensure listener is up and running etc and get rid of the supervisor
  %%       structure
  case whereis(dbg_iserver) of
    undefined -> dbg_iserver:start();
    _         -> ok
  end.

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
