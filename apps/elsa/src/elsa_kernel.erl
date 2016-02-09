
-module(elsa_kernel).

-export([init/3,
         handle/2,
         process/2,
         terminate/3
         ]).

-record(kernel, {method, service, version, endpoint, headers, body, timeout}).

init({tcp, http}, Req, _Opts) ->
  lager:info("Service request received: ~w", [Req]),
  {ok, Req, undefined}.

handle(Req, State) ->
  {Kernel, Request} = extract_kernel(Req),
  Response = case can_process(Kernel, Request) of
    true -> monitor(Kernel);
    false -> {404}
  end,
  {ok, Response, State}.

extract_kernel(Req) ->
  {Version, Req1} = cowboy_req:binding(version, Req),
  {Service, Req2} =  cowboy_req:binding(service, Req1),
  {Endpoint, Req3} = cowboy_req:path(Req2),
  {Timeout, Req4} = cowboy_req:header(<<"x-elsa-timeout">>, Req3, <<"5000">>),
  {Headers, Req5} = cowboy_req:headers(Req4),
  {Method, Req6} = cowboy_req:method(Req5),
  {ok, Body, Request} = cowboy_req:body(Req6, [{length, infinity}]),
  {#kernel{method=Method
         , service=Service
         , version=Version
         , endpoint=elsa_handler:truncate(Version, Service, Endpoint)
         , headers=Headers
         , body=Body
         , timeout=binary_to_integer(Timeout)}, Request}.

can_process(#kernel{service=Service, version=Version}, Req) ->
  true.

monitor(Kernel = #kernel{method=Method, service=Service, version=Version, endpoint=Endpoint, timeout=Timeout}) ->
  lager:info("Request method: ~s, service: ~s, version: ~s, endpoint: ~s created.", [Method, Service, Version, Endpoint]),
  Conn = spawn_link(?MODULE, process, [self(), Kernel]),
  receive
    RESPONSE -> RESPONSE
  after Timeout ->
    ID = elsa_task:new(self(), Service, Version, Method, Endpoint),
    Conn ! {timeout, ID},
    elsa_handler:task(Service, Version, ID)
  end.

process(Monitor, Kernel) ->
  RESPONSE = call(Kernel),
  receive
    {timeout, ID} -> elsa_task:store(ID, RESPONSE)
  after 0 ->
    Monitor ! RESPONSE
  end.

call(Kernel = #kernel{method=Method, service=Service, version=Version, headers=Headers, body=Body, endpoint=Endpoint}) ->
  Instance = elsa_service:checkout(Service, Version),
  URL = <<Instance/binary, Endpoint/binary>>,
  case elsa_http_client:call(Method, URL, Headers, Body) of
    {ok, Status, RespHeaders, RespBody} ->
      elsa_service:checkin(Service, Version, Instance),
      {Status, RespHeaders, RespBody};
    retry ->
      elsa_service:checkin(Service, Version, Instance),
      lager:error("Connection to ~s for service: ~s version: ~s method: ~s endpoint: ~s failed", [Instance, Service, Version, Method, URL]),
      call(Kernel)
  end.

terminate(_Reason, Req, State) ->
  ok.
