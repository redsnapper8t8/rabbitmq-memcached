-define(VERSION, "0.1").

-define(COMMAND_TIMEOUT, 120000).

-record(storage, {key, flags, exptime, bytes}).
-record(deletion, {key, time}).
