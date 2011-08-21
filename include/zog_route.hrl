-type tokenized_string() :: [string()].
-type tokenized_domain() :: tokenized_string().
-type tokenized_path()   :: tokenized_string().
-type function_name()    :: atom().
-type strategy()         :: authenticate | authorize.

-record(zog_route, {hostname
                      :: [tokenized_domain()],
                    handler_module
                      :: module(),
                    default_path_function = default_page
                      :: function_name() | function(),
                    default_subdomain_function = default_subdomain
                      :: function_name() | function(),
                    extra_args = []
                      :: [{atom(), any()}],
                    path_mods = []
                      :: [{[tokenized_path()], module()}],
                    path_strategies = []
                      :: [{strategy(), list()}]}).

-record(zog_route_strategy, {hostpath
                               :: {tokenized_domain(), tokenized_path()},
                             strategy = '_'
                               :: strategy(),
                             failure_function = '_'
                               :: function_name() | function(),
                             run_function = '_'
                               :: function_name() | function()}).
