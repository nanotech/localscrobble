# localscrobble

A scrobbling server for localhost, implementing
[last.fm's 1.2.1 protocol][protocol].

[protocol]: https://web.archive.org/web/20190531021725/https://www.last.fm/api/submissions

## Building

    cabal new-build

or

    stack build

Both commands will print the path of the compiled binary when
complete.

## Usage

    localscrobble [--host HOST] [--port PORT] [--database PATH] [--accounts PATH]

    Available options:
      --host HOST              Host to listen on (default: "127.0.0.1")
      --port PORT              Port to listen on (default: 7123)
      --database PATH          Database path (default: "localscrobble.db")
      --accounts PATH          Account list path (default: "localscrobble-accounts.txt")
      -h,--help                Show this help text

The accounts file is a list of allowed username and password
pairs in this format:

    user1:password1
    user2:password2

If your scrobbling client supports submitting to an alternate
server, set it to submit to `http://127.0.0.1:7123/`. If it
doesn't, point `post.audioscrobbler.com` to `127.0.0.1` in your
`/etc/hosts` and run a reverse proxy on port 80. For example,

    echo "127.0.0.1 post.audioscrobbler.com" | sudo tee -a /etc/hosts
    sudo socat TCP-LISTEN:80,fork TCP:127.0.0.1:7123

## Database

Scrobbles will be written to an SQLite database at
`./localscrobble.db`. Some useful queries to run are in the
analysis directory:

    sqlite3 localscrobble.db < analysis/top-tracks.sql | less
    sqlite3 localscrobble.db < analysis/list-scrobbles.sql | less

To write your own queries, you can view the database's schema with

    sqlite3 localscrobble.db .schema
