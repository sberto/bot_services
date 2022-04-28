bot_services
=====

An OTP application

Build
-----

Generate the token config file:
``` /bin/sh
$ support/insert_token.sh
```

Start the application:
``` /bin/sh
$ rebar3 release as prod && _build/default/rel/bot_services/bin/bot_services console 
```
