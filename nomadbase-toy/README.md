
# Just a wrapper

This module's sole purpose is to provide an executable containing the code from the library.

The `nomadbase-toy` executable provides a server that only communicates with one client, you, on the command line.

Some examples:

```
Subscribe { path = Path ["myApp", "hosts"] }

Put { path = Path ["myApp", "hosts", "localhost:1346"], putNewValue = Just "hi" }

Put { path = Path ["myApp", "hosts", "localhost:7353"], putNewValue = Just "hi" }

GetChildren { path = Path ["myApp", "hosts"] }

Unsubscribe { path = Path ["myApp", "hosts"] }

Put { path = Path ["myApp", "hosts", "localhost:8898"], putNewValue = Just "hi" }
```
