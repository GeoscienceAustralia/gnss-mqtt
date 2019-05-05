see `rebar.config` for dependencies on `aws-erlang` and `vernemq_dev`.

compile plugin:

```
$ rebar3 compile
```

to interactive with the compiled code (and its plugins) via erlang repl:

```
$ rebar3 shell
```

make sure hackney, and its dependencies, has been fully loaded into the repl (needed for AWS API calls):
```
> application:ensure_all_started(hackney).
```

create an aws client (note the double angle brackets for casting strings to binary)
```
> Client = aws_client:make_client(<<"your-aws-key">>, <<"your-aws-secret">>, <<"ap-southeast-2">>).
```

list the available user pools (note the hashmap and the API's required 'MaxResults' field):
```
> aws_cognito_idp:list_user_pools(Client, #{<<"MaxResults">> => 20}, []).
{ok,#{<<"UserPools">> =>
          [#{<<"CreationDate">> => 1544050958.223,
             <<"Id">> => <<"ap-southeast-2_Vr0EvkjSQ">>,
             <<"LambdaConfig">> => #{},
             <<"LastModifiedDate">> => 1550716104.605,
             <<"Name">> => <<"test-pool">>}]},
    {200,
     [{<<"Date">>,<<"Fri, 03 May 2019 06:25:55 GMT">>},
      {<<"Content-Type">>,<<"application/x-amz-json-1.1">>},
      {<<"Content-Length">>,<<"154">>},
      {<<"Connection">>,<<"keep-alive">>},
      {<<"x-amzn-RequestId">>,
       <<"4e705169-6d6c-11e9-b861-67e1658ea973">>}],
     #Ref<0.3345765125.3976462337.179074>}}
```

try to initiate the authentication of a user, as an admin

```
> aws_cognito_idp:admin_initiate_auth(Client, #{<<"UserPoolId">> => <<"your-cognito-user-pool-id">>, <<"ClientId">> => <<"your-cognito-client-id">>, <<"AuthFlow">> => <<"ADMIN_NO_SRP_AUTH">>, <<"AuthParameters">> => #{<<"USERNAME">> => <<"testuser">>, <<"PASSWORD">> => <<"P@ssword01">>}}, []).
```

this will return something of the form `{ok, ..., ...}` if user is in the pool, or `{error, ..., ...}` if it is not
