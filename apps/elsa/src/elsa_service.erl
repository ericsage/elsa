
-module(elsa_service).

-export([all/0,
         versions/1,
         instances/2,
         instance/3,
         register/1,
         unregister/1,
         checkout/3,
         checkin/3,
         available/2,
         count/2]).

-spec all() -> [] | [any()].
all() ->
  elsa_service_database:all().

-spec versions(binary()) -> [] | [any()].
versions(Service) when is_binary(Service) ->
  elsa_service_database:versions(Service).

-spec instances(binary(), binary()) -> [] | [any()].
instances(Service, Version) ->
  case elsa_instance_database:exists(Service, Version) of
    false -> [];
    true -> elsa_instance_database:all(Service, Version)
  end.

instance(Service, Version, Location) ->
  elsa_instance_database:retreive(Service, Version, Location).

register(Registration) ->
  elsa_registry:register(Registration).

unregister(Registration) ->
  elsa_registry:unregister(Registration).

checkout(Service, Version, Delay) ->
  case elsa_service_worker:checkout(Service, Version) of
    unavailable ->
      timer:sleep(Delay),
      checkout(Service, Version, Delay);
    Instance -> Instance
  end.

checkin(Service, Version, Instance) ->
  lager:info("Checking in instance of service: ~s", [Service]),
  elsa_service_worker:checkin(Service, Version, Instance).

available(Service, Version) ->
  case elsa_instance_database:exists(Service, Version) of
    false -> false;
    true ->
      case elsa_service_worker:find(Service, Version) of
        undefined ->
          elsa_service_worker_sup:start_child(Service, Version);
        _ -> ok
      end,
      case count(Service, Version) of
        0 -> false;
        _ -> true
      end
  end.

count(Service, Version) ->
  {service, _, _, I, _, _} = elsa_service_database:retreive(Service, Version),
  I.
