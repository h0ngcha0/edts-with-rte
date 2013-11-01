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

-behaviour(edts_plugin).

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
  io:format("rte_run args: ~p~n", [Args]),
  edts_rte_server:rte_run(Module, Func, Args).

interpret_module(Module) ->
  edts_rte_int_listener:interpret_module(Module).

uninterpret_module(Module) ->
  edts_rte_int_listener:uninterpret_module(Module).

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
%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
