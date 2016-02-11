-module(elsa_service_database).

-export([load/0,
         clear/0,
         create/2,
         instance_added/3,
         instance_removed/3,
         instance_in/4,
         instance_out/2,
         retreive/2,
         all/0,
         versions/1
        ]).

-include_lib("stdlib/include/qlc.hrl").

-record(service, {version :: {binary(), binary()},
                  created :: {integer(), integer(), integer()},
                  instances :: integer(),
                  unreachable :: 0,
                  unreachable_capacity :: {integer(), integer()},
                  registered_capacity :: {integer(), integer()},
                  out = 0
}).

-spec load() -> ok.
load() ->
  case elsa_table:create(elsa_services, service, record_info(fields, service)) of
    ok -> lager:info("Services table created");
    exists -> lager:error("Services table already exists")
  end,
  ok.

-spec clear() -> ok.
clear() ->
  case elsa_table:clear(elsa_services) of
    aborted -> lager:info("Service table could not be cleared.");
    ok -> lager:info("Service table cleared.")
  end,
  ok.

-spec create(binary(), binary()) -> ok.
create(Name, Version) ->
  update(Name, Version, os:timestamp(), 0, 0, {0, 0}, {0,0}, 0).

-spec instance_added(binary(), binary(), integer() | atom()) -> ok.
instance_added(Name, Version, Capacity) when is_integer(Capacity) ->

  {service, V, T, I, U, UR, {B, U}, O} = retreive(Name, Version),
  update(Name, Version, T, I+1, U, UR, {B+Capacity, U}, O);
instance_added(Name, Version, Capacity) when is_atom(Capacity) ->
  {service, V, T, I, U, UR, {B, U}, O} = retreive(Name, Version),
  update(Name, Version, T, I+1, U, UR, {B, U+1}, O).

-spec instance_removed(binary(), binary(), integer() | atom()) -> ok.
instance_removed(Name, Version, Capacity) when is_integer(Capacity) ->
  {service, _, T, I, U, UR, {B, U}, O} = retreive(Name, Version),
  update(Name, Version, T, I-1, U, UR, {B-Capacity, U}, O);
instance_removed(Name, Version, Capacity) when is_atom(Capacity) ->
  {service, _, T, I, U, UR, {B, U}, O} = retreive(Name, Version),
  update(Name, Version, T, I-1, U, UR, {B, U-1}, O).

instance_in(Name, Version, Capacity, Available) when is_integer(Capacity) ->
  {service, _, T, I, U, {B, U}, RC, O} = retreive(Name, Version),
  case Available of
    true ->
      update(Name, Version, T, I, U, {B, U}, RC, O+1);
    false ->
      update(Name, Version, T, I, U+1, {B, U+Capacity}, RC, O+1)
  end;
instance_in(Name, Version, Capacity, Available) when is_integer(Capacity) ->
  {service, _, T, I, U, {B, U}, RC, O} = retreive(Name, Version),
  case Available of
    true ->
      update(Name, Version, T, I, U, {B, U}, RC, O+1);
    false ->
      update(Name, Version, T, I, U+1, {B, U+1}, RC, O+1)
  end.

-spec instance_out(binary(), binary()) -> ok.
instance_out(Name, Version) ->
  {service, V, T, I, U, UC, RC, O} = retreive(Name, Version),
  update(Name, Version, T, I, U, UC, RC, O-1).

update(Name, Version, Created, Instance, Unreachable, U_C, R_Capacity, Out) ->
  Service = #service{version={Name, Version},
                     created=Created,
                     instances=Instance,
                     unreachable=Unreachable,
                     unreachable_capacity=U_C,
                     registered_capacity=R_Capacity,
                     out=Out},
  elsa_table:action(elsa_services, Service, write, write).

-spec retreive(binary(), binary()) -> not_found | binary().
retreive(Name, Version) ->
  case elsa_table:action(elsa_services, {Name, Version}, read, read) of
    [Service] -> Service;
    [] -> not_found
  end.

-spec all() -> [] | [#service{}].
all() ->
  elsa_table:do(qlc:q([S || S <- mnesia:table(elsa_services)])).

-spec versions(binary()) -> [] | [#service{}].
versions(Service) ->
  elsa_table:do(qlc:q([S || S <- mnesia:table(elsa_services), is_service(S#service.version, Service)])).

is_service({S, _}, Service) ->
  S == Service.
