-module(zog_cxn).

% We *could* use -extends(mochiweb_request) here, but erlang extends support
% isn't competely operational *and* it handles missing methods using
% error_handler causing more overhead than seems necessary.
% For now, we just wrap all the mochiweb_request methods.

% connection identity exports
-export([extra/2]).
-export([uid/1, secret/1, logged_in/1, login/2, logout/1]).
-export([permission/3]).

% mochiweb_request wrapper exports
-export([new/4]).
-export([get_header_value/2, get_primary_header_value/2, get_combined_header_value/2, get/2, dump/1]).
-export([send/2, recv/2, recv/3, recv_body/1, recv_body/2, stream_body/4]).
-export([start_response/2, start_response_length/2, start_raw_response/2]).
-export([respond/2, ok/2]).
-export([not_found/1, not_found/2]).
-export([parse_post/1, parse_qs/1]).
-export([should_close/1, cleanup/1]).
-export([parse_cookie/1, get_cookie_value/2]).
-export([serve_file/3, serve_file/4]).
-export([accepted_encodings/2]).
-export([accepts_content_type/2, accepted_content_types/2]).

%%%----------------------------------------------------------------------
%%% instance issues
%%%----------------------------------------------------------------------
extra(What,
    {?MODULE, [_, Extra, _, _]}) ->
  proplists:get_value(What, Extra, []).

%%%----------------------------------------------------------------------
%%% user info / identity of connection
%%%----------------------------------------------------------------------
logged_in({?MODULE, [_, _, AuthenticationModule, _]}=THIS) ->
  AuthenticationModule:logged_in(THIS).

uid({?MODULE, [_, _, AuthenticationModule, _]}=THIS) ->
  AuthenticationModule:session_uid(THIS).

secret({?MODULE, [_, _, AuthenticationModule, _]}=THIS) ->
  AuthenticationModule:session_secret(THIS).

login(Uid, {?MODULE, [_, _, AuthenticationModule, _]}=THIS) ->
  AuthenticationModule:set_session(THIS, Uid).

logout({?MODULE, [_, _, AuthenticationModule, _]}=THIS) ->
  AuthenticationModule:remove_session(THIS).

% Cxn:permission(user, superadmin, <<"entiresite">>) = true | false.
% Cxn:permission(content, owner, <<"post:id:64">>) = true | false.
permission(AccessSpace, What, {?MODULE, [_, _, _, AuthorizationModule]}=THIS) ->
  AuthorizationModule:access(AccessSpace, What, THIS:uid()).

%%%----------------------------------------------------------------------
%%% wrappers around mochiweb_request for our Req
%%%----------------------------------------------------------------------
% We don't care about the types of anything here.  These just pass through.
new(A, B, C, D) -> {?MODULE, [A, B, C, D]}.
get_header_value(A, {?MODULE, [Req, _, _, _]}) -> Req:get_header_value(A).
get_combined_header_value(A,
    {?MODULE, [Req, _, _, _]}) -> Req:get_combined_header_value(A).
get_primary_header_value(A,
    {?MODULE, [Req, _, _, _]}) -> Req:get_primary_header_value(A).
get(A, {?MODULE, [Req, _, _, _]}) -> Req:get(A).
dump({?MODULE, [Req, _, _, _]}) -> Req:dump().
send(A, {?MODULE, [Req, _, _, _]}) -> Req:send(A).
recv(A, {?MODULE, [Req, _, _, _]}) -> Req:recv(A).
recv(A, B, {?MODULE, [Req, _, _, _]}) -> Req:recv(A, B).
recv_body({?MODULE, [Req, _, _, _]}) -> Req:recv_body().
recv_body(A,
    {?MODULE, [Req, _, _, _]}) -> Req:recv_body(A).
stream_body(A, B, C,
    {?MODULE, [Req, _, _, _]}) -> Req:stream_body(A, B, C).
start_response(A,
    {?MODULE, [Req, _, _, _]}) -> Req:start_response(A).
start_response_length(A,
    {?MODULE, [Req, _, _, _]}) -> Req:start_response_length(A).
start_raw_response(A,
    {?MODULE, [Req, _, _, _]}) -> Req:start_raw_response(A).
respond(A,
    {?MODULE, [Req, _, _, _]}) -> Req:respond(A).
ok(A,
    {?MODULE, [Req, _, _, _]}) -> Req:ok(A).
not_found({?MODULE, [Req, _, _, _]}) -> Req:not_found().
not_found(A,
    {?MODULE, [Req, _, _, _]}) -> Req:not_found(A).
parse_post({?MODULE, [Req, _, _, _]}) -> Req:parse_post().
parse_qs({?MODULE, [Req, _, _, _]}) -> Req:parse_qs().
should_close({?MODULE, [Req, _, _, _]}) -> Req:should_close().
cleanup({?MODULE, [Req, _, _, _]}) -> Req:cleanup().
parse_cookie({?MODULE, [Req, _, _, _]}) -> Req:parse_cookie().
get_cookie_value(A,
    {?MODULE, [Req, _, _, _]}) -> Req:get_cookie_value(A).
serve_file(A, B,
    {?MODULE, [Req, _, _, _]}) -> Req:serve_file(A, B).
serve_file(A, B, C,
    {?MODULE, [Req, _, _, _]}) -> Req:serve_file(A, B, C).
accepted_encodings(A,
    {?MODULE, [Req, _, _, _]}) -> Req:accepted_encodings(A).
accepts_content_type(A,
    {?MODULE, [Req, _, _, _]}) -> Req:accepts_content_type(A).
accepted_content_types(A,
    {?MODULE, [Req, _, _, _]}) -> Req:accepted_content_types(A).
