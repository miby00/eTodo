%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 16 Jun 2012 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
{application, 'ePort',
 [{description, "EPort an RPC over socket server."},
  {vsn, "0.9.0"},
  {modules, [
	     ePort,
	     ePortListener
	     ]},
  {maxT, infinity},
  {registered, []},
  {applications, [kernel, stdlib]},
  {included_applications, []},
  {env, []}
 ]
}.
