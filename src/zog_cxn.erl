-module(zog_cxn, [Req, Extra,
                  AuthenticationModule, AuthorizationModule]).
% We *could* use -extends(mochiweb_request) here, but erlang extends support
% isn't competely operational *and* it handles missing methods using
% error_handler causing more overhead than seems necessary.
% For now, we just wrap all the mochiweb_request methods.

% connection identity exports
-export([extra/1]).
-export([uid/0, secret/0, logged_in/0, login/1, logout/0]).
-export([permission/2]).

% mochiweb_request wrapper exports
-export([get_header_value/1, get_primary_header_value/1, get/1, dump/0]).
-export([send/1, recv/1, recv/2, recv_body/0, recv_body/1, stream_body/3]).
-export([start_response/1, start_response_length/1, start_raw_response/1]).
-export([respond/1, ok/1]).
-export([not_found/0, not_found/1]).
-export([parse_post/0, parse_qs/0]).
-export([should_close/0, cleanup/0]).
-export([parse_cookie/0, get_cookie_value/1]).
-export([serve_file/2, serve_file/3]).
-export([accepted_encodings/1]).
-export([accepts_content_type/1, accepted_content_types/1]).

%%%----------------------------------------------------------------------
%%% instance issues
%%%----------------------------------------------------------------------
extra(What) ->
  proplists:get_value(What, Extra, []).

%%%----------------------------------------------------------------------
%%% user info / identity of connection
%%%----------------------------------------------------------------------
logged_in() ->
  AuthenticationModule:logged_in(THIS).

uid() ->
  AuthenticationModule:session_uid(THIS).

secret() ->
  AuthenticationModule:session_secret(THIS).

login(Uid) ->
  AuthenticationModule:set_session(THIS, Uid).

logout() ->
  AuthenticationModule:remove_session(THIS).

% Cxn:permission(user, superadmin, <<"entiresite">>) = true | false.
% Cxn:permission(content, owner, <<"post:id:64">>) = true | false.
permission(AccessSpace, What) ->
  AuthorizationModule:access(AccessSpace, What, THIS:uid()).

%%%----------------------------------------------------------------------
%%% wrappers around mochiweb_request for our Req
%%%----------------------------------------------------------------------
% We don't care about the types of anything here.  These just pass through.
get_header_value(A) -> Req:get_header_value(A).
get_primary_header_value(A) -> Req:get_primary_header_value(A).
get(A) -> Req:get(A).
dump() -> Req:dump().
send(A) -> Req:send(A).
recv(A) -> Req:recv(A).
recv(A, B) -> Req:recv(A, B).
recv_body() -> Req:recv_body().
recv_body(A) -> Req:recv_body(A).
stream_body(A, B, C) -> Req:stream_body(A, B, C).
start_response(A) -> Req:start_response(A).
start_response_length(A) -> Req:start_response_length(A).
start_raw_response(A) -> Req:start_raw_response(A).
respond(A) -> Req:respond(A).
ok(A) -> Req:ok(A).
not_found() -> Req:not_found().
not_found(A) -> Req:not_found(A).
parse_post() -> Req:parse_post().
parse_qs() -> Req:parse_qs().
should_close() -> Req:should_close().
cleanup() -> Req:cleanup().
parse_cookie() -> Req:parse_cookie().
get_cookie_value(A) -> Req:get_cookie_value(A).
serve_file(A, B) -> Req:serve_file(A, B).
serve_file(A, B, C) -> Req:serve_file(A, B, C).
accepted_encodings(A) -> Req:accepted_encodings(A).
accepts_content_type(A) -> Req:accepts_content_type(A).
accepted_content_types(A) -> Req:accepted_content_types(A).
