# ![eTodo](https://rawgit.com/miby00/eTodo/master/priv/www/docs/graphics/eTodo.png)
eTodo is a P2P task manager written in Erlang. 

Key functionality include:

* Tasks that can be shared between peers
* Communication between peers using encrypted chat
* Task progress that can be shown to non eTodo users using “link view” functionality
* A To-Do list in eTodo has a default order
* It is easy to move tasks up and down within a list
* A task can be in more than one list
* A task can have one or more sub tasks, and so can sub tasks.
* Non intrusive reminders
* Local storage (no central server)

## Installing eTodo as a release
I have made prebuilt binary releases of eTodo for a few OS I run at home:

* [Mac 64-bit](https://cdn.rawgit.com/miby00/eTodo-releases/v.1.0.0/Mac/eTodo.dmg)
* [Windows 32-bit](https://cdn.rawgit.com/miby00/eTodo-releases/v.1.0.0/Windows%207/eTodo.exe)
* [Ubuntu 64-bit](https://cdn.rawgit.com/miby00/eTodo-releases/v.1.0.0/Ubuntu/etodo_1.0.0_amd64.deb)

If no prebuilt package is available for your platform you can download and run eTodo according to 
the instructions below.

## Installation

eTodo require an Erlang installation with a working wxErlang environment. 

I recommend using Erlang Solutions [installation packages](https://www.erlang-solutions.com/resources/download.html).

### Download, build and run eTodo

Feel free to ignore steps already taken ;-)

* Install [Erlang](https://https://www.erlang-solutions.com/resources/download.html)
* Install [rebar3](https://github.com/erlang/rebar3)
* Install [Gleam](https://gleam.run/getting-started/installing/) for the Mist/Lustre web backend
* Install [GIT](https://git-scm.com/downloads)
* Download code using GIT, use the command:

        $ git clone https://github.com/miby00/eTodo.git
        
**Before you run eTodo**: enter the config folder and edit the *sys.config* file.
Set the *dir* and *logDir* configurations.

  Do one of the following (in the root folder of the project):
  * Build a release using the command (Gleam dependencies are locked in `web/backend/manifest.toml`):
  
        $ make release
  * Run a rebar shell using the command:
   
        $ make gleam-backend && rebar3 shell
        
If you made a release the release can be found in the folder:

    ./_build/default/rel/eTodo
    
To run the release enter the bin folder of the release and use the command:

    ./eTodo
    
Have patience: The first startup needs to create an empty TODO database.

### Mist/Lustre web backend

The Mist/Lustre backend is a separate Gleam project in `web/backend`. `make release`
builds it for the Erlang target and lets relx resolve its OTP applications from
the Gleam build output. Mist and its transitive Gleam dependencies are resolved
by Gleam and pinned in `web/backend/manifest.toml`.

The new web interface is opt-in and binds to `127.0.0.1` on port `9003` by
default. It serves HTTPS only. To enable it, add the following entries to the
`eTodo` application settings in `config/sys.config`:

    {mistWebEnabled, true},
    {mistWebPort, 9003},
    {mistWebBind, "127.0.0.1"},
    {mistWebCert, "server_cert.pem.fullchain.pem"},
    {mistWebKey, "server_key.pem"}

If neither `server_cert.pem` nor `server_key.pem` exists in `priv`, eTodo
creates a self-signed certificate (valid for 10 years) on first start, so a
fresh install works without setup. Browsers will warn about it; replace the
files with a real certificate to avoid that. Existing files are never
overwritten.

Certificate and key filenames are resolved relative to the application's
`priv` directory; absolute paths are also accepted. The certificate file must
contain the complete chain for browsers to trust it. Use `"0.0.0.0"` as
`mistWebBind` to listen on all interfaces, and expose the port only through a
trusted firewall or reverse proxy.

### Reaching the web UI through another peer

Behind a firewall, another peer in the circle can relay the connection. The
relay only copies the bytes of the browser's TLS connection to the web UI of
the owner through the existing peer connection, so it cannot read or change
anything. The browser checks the certificate of the owner. A desktop client
knows its account once logged in; a headless node needs `user` in `sys.config`.

A typical setup is a headless server as the relay for a desktop client at work:
the server gets `webRelayPort` and `webRelayTarget` (the account of the work
client), and the work client enables the web UI (`mistWebEnabled`), lists the
server's account in `webTunnelPeers` and sets `mistWebLinkHost` to the server's
address. "Make proxy link" in the desktop client then copies the link.

On the relay peer (the browser connects to this port):

    {webRelayPort, 9005},
    {webRelayTarget, "OwnerUser"}

On the owner (only the listed relay peers may open tunnels):

    {webTunnelPeers, ["RelayUser"]}

The certificate of the owner must also list the relay's hostname, otherwise
the browser warns about a name mismatch. Optionally set
`{mistWebLinkHost, "relayhost:9005"}` on the owner so that "Link to file" and
"Link to view" use the relay address. Each node allows 50 tunnels, and idle
tunnels close after ten minutes.

The login uses eTodo's existing credentials: the local account's web password,
or the circle password for a peer account that already shares todos with the
local account. Successful logins receive an eight-hour `Secure`, `HttpOnly`,
`SameSite=Strict` session cookie. The authenticated page shows the todo list
using eTodo's existing per-user list membership and ordering. Each task is one
compact row; its ▾ icon expands all task details. The local account (and only
that account) can tick a task as done, or untick it to set it to Planning; peer
logins are read-only, and writes require a CSRF token. The local account can
also edit a task's status, priority, due date, progress, time estimates,
description and comment, choose which lists it belongs to (a task can be in
several lists, and is always in "All tasks"), or delete it. The last viewed todo list is remembered
in a cookie. The Status page (`/status`) is a full screen display of your
status, status message and countdown timer, green when Available, red when
Busy, which can be left open on a screen so others can see when you can answer
questions. The local account can also change its status (Available, Busy,
Away) and status message there; the desktop client, if running, follows and
peers are told. The ⚙ Settings page offers a filter (status,
priority) and primary/secondary sort settings,
stored in the same per-user `webSettings` as the old client. Live updates are
not implemented yet. The new server does not replace the existing
web server. Its responsive interface uses locally served Bootstrap components,
eTodo styling and a tiny same-origin script that updates the checkbox in place
(without it, the form falls back to a normal page reload); it does not depend on
a CDN.

[Getting started with eTodo](https://rawgit.com/miby00/eTodo/master/priv/www/docs/eTodo.html)

### Status

eTodo isn't finished, when I feel inspired I code on it during my spare time.
So do not expect it to be perfect, commented, have test cases or be fully documented ;)

### Contributors

I would like to give special thanks to:

* [Anders Ramsell](https://github.com/andersramsell)
* [Gunnar Sverredal](https://github.com/donGunnar)

For contributing to the project.
