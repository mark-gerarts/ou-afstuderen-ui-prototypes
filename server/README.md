# Server

In order to test the various UI prototypes, we need a simple webserver that
handles some specific AJAX requests. This projects runs a very simple server
that accepts any POST requests, and returns an album in JSON format at the
endpoint `/album/1`.

To spin it up:

```
$ # In a separate terminal window
$ nix-shell --run start-server
```

The server is then accessible at localhost:8080.
