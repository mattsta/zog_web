-module(zog_page).

% site determination
-export([is_at_host/2, is_at_uri/2, is_using_protocol/2]).
-export([force_site/4, perm_redirect_immediate/3, temp_redirect_immediate/3]).

% set regular/ssl cookies
-export([regular_cookie/4, regular_cookie/5]).
-export([secure_cookie/4, secure_cookie/5]).
-export([make_state_cookie/2, make_state_cookie/3]).

% expire/delete cookies
-export([regular_cookie_expire/2, regular_cookie_expire/3]).
-export([secure_cookie_expire/2, secure_cookie_expire/3]).

% Cookie retrieval
-export([cookie/2]).

% Form retrieval
-export([form_postvars/2, form_queryvars/2]).

% request type determination
-export([is_options/1, is_get/1, is_head/1,
         is_post/1, is_put/1, is_delete/1,
         is_trace/1, is_connect/1]).

% intention determination
-export([is_ssl/1, is_xhr/1, is_dnt/1]).

% Responding to requests
-export([ok/2, ok/3, idle_ping/1, deny/2, perm_redirect/2, temp_redirect/2]).

-define(SRV_HDR, "zogalot").
-define(CONTENT_TYPE, "text/html; charset=utf-8").

%%%----------------------------------------------------------------------
%%% forced site handling
%%%----------------------------------------------------------------------
-spec is_at_host(any(), string()) -> boolean().
is_at_host(Req, Host) ->
  case Req:get_header_value("host") of
    Host -> true;
       _ -> false
  end.

-spec is_at_uri(any(), string()) -> boolean().
is_at_uri(Req, URI) ->
  case Req:get(path) of
    URI -> true;
      _ -> false
  end.

-spec is_using_protocol(any(), http | https | string()) -> boolean().
is_using_protocol(Req, "https") -> is_using_protocol(Req, https);
is_using_protocol(Req, "http")  -> is_using_protocol(Req, http);
is_using_protocol(Req, Proto) when is_atom(Proto) ->
  IsSSL = is_ssl(Req),
  IsNotSSL = not IsSSL,
  case Proto of
    https -> IsSSL;
    http  -> IsNotSSL
  end.

-spec force_site(any(), atom() | string(), string(), string()) ->
  ok | no_return().
force_site(Req, Proto, Host, URI) ->
  % if (andalso (is-at-host Req Host)
  %             (is-at-URI Req URI)
  %             (is-using-protocol Req Proto)) -> ok;
  Redirect = fun() -> perm_redirect_immediate(Proto, Host, URI) end,
  case is_at_host(Req, Host) of
     true -> case is_at_uri(Req, URI) of
               true  -> case is_using_protocol(Req, Proto) of
                          true  -> ok;
                          false -> Redirect()
                        end;
               false -> Redirect()
             end;
    false -> Redirect()
  end.

-spec perm_redirect_immediate(atom() | string(), string(), string()) ->
  no_return().
perm_redirect_immediate(Proto, Host, URI) ->
  redirect_immediately(perm_redirect, Proto, Host, URI).

-spec temp_redirect_immediate(atom() | string(), string(), string()) ->
  no_return().
temp_redirect_immediate(Proto, Host, URI) ->
  redirect_immediately(temp_redirect, Proto, Host, URI).

redirect_immediately(Type, Proto, Host, URI) ->
  URL = io_lib:format("~s://~s~s", [Proto, Host, URI]),
  throw({Type, URL}).

%%%----------------------------------------------------------------------
%%% cookie handling
%%%----------------------------------------------------------------------
-define(MINUTE_SECONDS, 60).
-define(HOUR_SECONDS, ?MINUTE_SECONDS * 60).
-define(DAY_SECONDS, ?HOUR_SECONDS * 24).

use_cookie_type(Req) ->
  case is_ssl(Req) of
    true  -> fun secure_cookie/5;
    false -> fun regular_cookie/5
  end.

regular_cookie(Name, Data, Length, Domain) ->
  cookie(Name, Data, regular, Length, Domain).
regular_cookie(Name, Data, Length, Domain, Path) ->
  cookie(Name, Data, regular, Length, Domain, Path).

secure_cookie(Name, Data, Length, Domain) ->
  cookie(Name, Data, secure, Length, Domain).
secure_cookie(Name, Data, Length, Domain, Path) ->
  cookie(Name, Data, secure, Length, Domain, Path).

regular_cookie_expire(Name, Domain) ->
  cookie(Name, expired, regular, expire, Domain).
regular_cookie_expire(Name, Domain, Path) ->
  cookie(Name, expired, regular, expire, Domain, Path).

secure_cookie_expire(Name, Domain) ->
  cookie(Name, expired, secure, expire, Domain).
secure_cookie_expire(Name, Domain, Path) ->
  cookie(Name, expired, secure, expire, Domain, Path).

cookie(Name, Data, Security, TimeLength, Domain) ->
  cookie(Name, Data, Security, TimeLength, Domain, "/").

cookie(Name, Data, Security, TimeLength, Domain, Path) ->
  Secure = case Security of
             secure -> {secure, true};
                  _ -> {http_only, true}
           end,
  Length = case TimeLength of
             year         -> ?DAY_SECONDS * 365;
             {years, N}   -> ?DAY_SECONDS * 365 * N;
             month        -> ?DAY_SECONDS * 31;
             {months, N}  -> ?DAY_SECONDS * 31 * N;
             day          -> ?DAY_SECONDS;
             {days, N}    -> ?DAY_SECONDS * N;
             hour         -> ?HOUR_SECONDS;
             {hours, N}   -> ?HOUR_SECONDS * N;
             minute       -> ?MINUTE_SECONDS;
             {minutes, N} -> ?MINUTE_SECONDS * N;
             expire       -> 0
           end,

  mochiweb_cookies:cookie(Name, Data,
    [{max_age, Length}, {domain, Domain}, {path, Path}, Secure]).

-spec cookie(any(), string() | [string()] | [atom()]) -> list() | [].
cookie(Req, Names) when is_list(Names) andalso is_atom(hd(Names)) ->
  cookie(Req, [atom_to_list(N) || N <- Names]);
cookie(Req, Name) when is_atom(Name) ->
  cookie(Req, atom_to_list(Name));
cookie(Req, Names) when is_list(Names) andalso is_list(hd(Names)) ->
  case Req:get_header_value("cookie") of
    undefined -> [undefined || _ <- Names];
      Cookies -> BakedCookies = mochiweb_cookies:parse_cookie(Cookies),
                 [case Name of
                    {CookieName, Default} -> proplists:get_value(Name,
                                               BakedCookies, Default);
                                     Name -> proplists:get_Value(Name,
                                               BakedCookies)
                  end || Name <- Names]
  end;
cookie(Req, {Name, Default}) when is_list(Name) ->
  case cookie(Req, [Name]) of
    [Result] -> Result;
          [] -> Default
  end.
cookie(Req, Name) when is_list(Name) ->
  case cookie(Req, [Name]) of
    [Result] -> Result;
          [] -> []
  end.

%%%----------------------------------------------------------------------
%%% private cookies
%%%----------------------------------------------------------------------
make_state_cookie(Req, CookieNames) ->
  make_state_cookie(Req, CookieNames, "/").

make_state_cookie(Req, CookieNames, Path) when is_list(CookieNames) ->
  CookieFun = use_cookie_type(Req),
  Site = host(Req),
  [CookieFun(X, guid(), {years, 2}, Site, Path) || X <- CookieNames];
make_state_cookie(Req, CookieName, Path) ->
  make_state_cookie(Req, [CookieName], Path).

guid() ->
  randhex(16).

randhex(Bytes) ->
  mochihex:to_hex(crypto:rand_bytes(Bytes)).

%%%----------------------------------------------------------------------
%%% form processing
%%%----------------------------------------------------------------------
form_vars([], Requested) -> [undefined || _ <- lists:seq(1, length(Requested))];
form_vars(_, []) -> [];
form_vars(FormVars, [{RequestedVar, Def} | Xs]) when is_atom(RequestedVar) ->
  form_vars(FormVars, [{atom_to_list(RequestedVar), Def} | Xs]);
form_vars(FormVars, [RequestedVar | Xs]) when is_atom(RequestedVar) ->
  form_vars(FormVars, [atom_to_list(RequestedVar) | Xs]);
form_vars(FormVars, [{RequestedVar, Default}|Xs]) when is_list(RequestedVar) ->
  [proplists:get_value(RequestedVar, FormVars, Default) |
    form_vars(FormVars, Xs)].
form_vars(FormVars, [RequestedVar | Xs]) when is_list(RequestedVar) ->
  [proplists:get_value(RequestedVar, FormVars) | form_vars(FormVars, Xs)].

form_postvars(Req, RequestedVars) ->
  PostVars = Req:parse_post(),
  form_vars(PostVars, RequestedVars).

form_queryvars(Req, RequestedVars) ->
  QueryVars = Req:parse_qs(),
  form_vars(QueryVars, RequestedVars).

%%%----------------------------------------------------------------------
%%% header helpers
%%%----------------------------------------------------------------------
-compile({inline, [{is_method, 2}]}).
-spec is_method(any(), atom()) -> boolean().
is_method(Req, Method) ->
  case Req:get(method) of
    Method -> true;
         _ -> false
  end.

% methods from http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html
% These were more useful before we put methods in-line with module callbacks.
is_options(Req) ->
  is_method(Req, 'OPTIONS').

is_get(Req) ->
  is_method(Req, 'GET').

is_head(Req) ->
  is_method(Req, 'HEAD').

is_post(Req) ->
  is_method(Req, 'POST').

is_put(Req) ->
  is_method(Req, 'PUT').

is_delete(Req) ->
  is_method(Req, 'DELETE').

is_trace(Req) ->
  is_method(Req, 'TRACE').

is_connect(Req) ->
  is_method(Req, 'CONNECT').

is_xhr(Req) ->
  case Req:get_header_value("x-requested-with") of
    undefined -> false;
            _ -> true
  end.

is_ssl(Req) ->
  case Req:get_header_value("x-forwarded-proto") of
    "https"   -> true;
    "http"    -> false;
    undefined -> case Req:get(scheme) of
                   https -> true;
                   http  -> false
               end
  end.

is_dnt(Req) ->
  DoNot1 = is_1(Req:get_header_value("x-do-not-track")),
  DoNot2 = is_1(Req:get_header_value("dnt")),
  DoNot1 orelse DoNot2.

-compile({inline, [{is_1, 1}]}).
is_1("1") -> true;
is_1(_) -> false.

host(Req) ->
  Req:get_header_value("host").

%%%----------------------------------------------------------------------
%%% sending things to clients
%%%----------------------------------------------------------------------
ok(Req, Body) ->
  Req:ok({?CONTENT_TYPE, [{"Server", ?SRV_HDR}], Body}).

ok(Req, Body, Cookies) ->
  Req:ok({?CONTENT_TYPE, [{"Server", ?SRV_HDR} | Cookies], Body}).

idle_ping(Req) ->
  Req:respond({204, [{"Server", "Idle Ping.  Wake up."}], []}).

deny(Req, Body) ->
  Req:respond({403, [{"Content-Type", ?CONTENT_TYPE},
                     {"Server", "Try harder to win me over."}], Body}).

perm_redirect(Req, To) -> redirect(Req, 301, To).
temp_redirect(Req, To) -> redirect(Req, 302, To).

redirect(Req, Method, To) ->
  Req:respond({Method, [{"Location", To},
                        {"Content-Type", ?CONTENT_TYPE}], ""}).
