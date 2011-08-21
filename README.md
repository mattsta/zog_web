zog_web: erlang-based web serving using domain routes
=====================================================

Usage
-----

### Basic Usage (ultra-quickstart)

* include `zog_web` as a rebar dependency
* start your erlang node with `-config myconfig` wherein you define
`web_ip`, `web_port`, and your `route_dirs` (see hnf for examples).
* `application:start(zog_web).`
* Now your webserver is running and serving traffic for the routes
listed in your configs under `route_dirs`.

### Example Application

A complete application using `zog_web` is available at
https://github.com/mattsta/hnf


Building
--------
Download deps:

        rebar get-deps

Build:

        rebar compile


Testing
-------
egad, we need tests.


Next Steps
----------
In no specific order:

* Actual documentation
* Tests
* Continual synchronization of features with upcoming zog components


When to use zog_web
-------------------
Use `zog_web` when you need a simple way to expose erlang code
to the Internet as a series of URLs.

Use `zog_web` when you want your entire web stack embedded in one application.
`zog_web` provides a web server, domain routing, and URL routing.  When paired
with `zog_user`, `zog_web` can automatically provide authentication and
authorization against pre-defined paths.


Background
----------
The zog project is a result of my experience deploying a dozen websites using
yaws over the past few years.  zog provides everyone with a low latency,
scalable, minimal, no-magic web framework that gets out of the way so you can
focus on getting your content to users.

zog consists of: zog_web, zog_user, zog_template, and a few applications
yet to be named (hint: it'll be zog_<something>).
Right now, only zog_web is available, but it's completely usable as an isolated
application.
The remaining applications are being polished/finished/integrated and should
be released soon.


Thanks
------

* mochiweb, for providing battle-tested web serving components.
* chicagoboss, from where I stole the idea of path routing and method arguments.
* lfe, for shoving lisp macros into erlang.
* and more!
