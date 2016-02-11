
-module(elsa_instance_database).

-export([
         load/2,
         clear/2,
         register/4,
         unregister/3,
         checkout/2,
         checkin/4,
         retreive/3,
         all/2,
         exists/2]).

-include_lib("stdlib/include/qlc.hrl").

-record(instance, {location :: binary(),
                   unreachable = false,
                   registered :: {integer(), integer(), integer()},
                   capacity = 0,
                   out = 0
}).

-spec load(binary(), binary()) -> ok.
load(Service, Version) ->
  Name = service(Service, Version),
  case elsa_table:create(Name, instance, record_info(fields, instance)) of
    ok -> lager:info("Service table ~s created.", [Name]);
    exists -> lager:error("Service table ~s already exists.", [Name])
  end,
  ok.

-spec clear(binary(), binary()) -> ok.
clear(Service, Version) ->
  Serv = service(Service, Version),
  case elsa_table:clear(Serv) of
    aborted -> lager:error("Service table ~s could not be cleared", [Serv]);
    ok -> lager:info("Service table ~s cleared.", [Serv])
  end,
  ok.

-spec register(binary(), binary(), binary(), integer()) -> #instance{}.
register(Service, Version, Location, Capacity) ->
  update(Service, Version, Location, false, os:timestamp(), Capacity, 0).

unregister(Service, Version, Location) ->
  elsa_table:action(service(Service, Version), Location, delete, write),
  Location.

-spec checkout(binary(), binary()) -> unavailable | binary().
checkout(Service, Version) ->
  case bound(Service, Version) of
    false ->
      case unbound(Service, Version) of
        false -> unavailable;
        Instance -> Instance
      end;
    Instance -> Instance
  end.

checkin(Service, Version, Location, Available) ->
  {instance, L, U, R, C, O} = retreive(Service, Version, Location),
  I = update(Service, Version, L, (not Available), R, C, O-1),
  I#instance.capacity.

status(Service, Version, Location, Unreachable) ->
  {instance, L, U, R, C, O} = retreive(Service, Version, Location),
  update(service, Version, L, Unreachable, R, C, O).

-spec retreive(binary(), binary(), binary()) -> not_found | binary().
retreive(Service, Version, Location) ->
  case elsa_table:action(service(Service, Version), Location, read, read) of
    [Instance] -> Instance;
    [] -> not_found
  end.

-spec all(binary(), binary()) -> [] | [#instance{}].
all(Service, Version) ->
  elsa_table:do(qlc:q([S || S <- mnesia:table(service(Service, Version))])).

exists(Service, Version) ->
  lists:member(service(Service, Version), mnesia:system_info(tables)).

update(Service, Version, Location, Unreachable, Registered, Capacity, Out) ->
  Instance = #instance{location=Location,
                       unreachable=Unreachable,
                       registered=Registered,
                       capacity=Capacity,
                       out=Out},
  elsa_table:action(service(Service, Version), Instance, write, write),
  Instance.

%% HELPERS

bound(Service, Version) ->
  Instances = elsa_table:do(qlc:q([I || I <- mnesia:table(service(Service, Version)),
    I#instance.capacity =/= infinity,
    I#instance.unreachable =/= true,
    I#instance.capacity =/= I#instance.out])),
  Sort = fun(X,Y) -> X#instance.out < Y#instance.out end,
  case lists:sort(Sort, Instances) of
    [] -> false;
    [{instance, L, U, R, C, O}|_] ->
      update(Service, Version, L, U, R, C, O+1),
      L
  end.

unbound(Service, Version) ->
  Instances = elsa_table:do(qlc:q([I || I <- mnesia:table(service(Service, Version)),
    I#instance.unreachable =/= true,
    I#instance.capacity == infinity])),
  Sort = fun(X,Y) -> X#instance.out < Y#instance.out end,
  case lists:sort(Sort, Instances) of
    [] -> false;
    [{instance, L, U, R, C, O}|_] ->
      update(Service, Version, L, U, R, C, O+1),
      L
  end.

service(Service, Version) -> binary_to_atom(<<<<"elsa_service__">>/binary, Service/binary, <<"_">>/binary, Version/binary >>, utf8).
