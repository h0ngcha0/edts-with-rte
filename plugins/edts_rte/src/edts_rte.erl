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
-export([ rte_run/4
        ]).

%% Behaviour exports
-export([edts_server_services/0,
         project_node_modules/0,
         project_node_services/0]).

%%%_* Defines ==================================================================
%%%_* API ======================================================================
rte_run(Node, Module, Func, Args) ->
  case edts_dist:call(Node, edts_rte_server, rte_run, [Module, Func, Args]) of
    {badrpc, _} -> {error, not_found};
    Result      -> Result
  end.

%% Behaviour callbacks
edts_server_services()  -> [].
project_node_modules()  ->
  [ edts_rte_app
  , edts_rte_util
  , edts_rte_int_listener
  , edts_rte_server
  , edts_rte_sup
  ].
project_node_services() -> [edts_rte_app].

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End: