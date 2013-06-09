embjson
=======

Parse transform library *embjson* allows using JSON syntax directly in Erlang sources.
Embedded JSON structure is transformed to Erlang term during compilation. The resulting
structure format depends on custom callback module.

Example of usage in unit tests
------------------------------

[Unit test](src/embjson_tests.erl) uses yaws json2 structure
[callback module](src/embjson_yaws_json2.erl):

```
-compile({parse_transform, embjson}).
-embjson([{callback, embjson_yaws_json2}, {function, '@json'}]).
```

Optional attribute *embjson* configures parse transformation:
* ``callback``: Name of a callback module (default is transformed module).
Module must implement ``embjson`` behaviour.
* ``function``: Function encapsulating embedded JSON (default is ``'@json'``).
Function call is replaced by term representing JSON structure passed as
parameter to the function call during compilation:
``Var = '@json'({"pink": "hippo"}) => Var = {struct, [{"pink", "hippo"}]}``
(if json2 format callback module is used).
