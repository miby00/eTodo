# eTodo
eTodo is a P2P task manager written in Erlang. 

Key functionality include:

* Tasks that can be shared between peers
* Communication between peers using chat
* Task progress that can be shown to non eTodo users using “link view” functionality
* A To-Do list in eTodo has a default order. It is easy to move tasks up and down within a list
* A task can be in more than one list
* A task can have one or more sub tasks, and so can sub tasks.
* Non intrusive reminders
* Local storage (no central server)

## Installation

eTodo require an Erlang installation with a working wxErlang environment. 

I recommend using Erlang Solutions [installation packages](https://www.erlang-solutions.com/downloads/download-erlang-otp).

### Download and run eTodo

1. Install Erlang
2. Download eTodo from github using the "download.ZIP" button
3. Extract the files
4. Go into the folder eTodo/priv
5. On Unix/Mac: run StartETodo.sh. On Windows: run StateETodo.bat

Have patience: The first startup needs to create an empty TODO database.

[To get started](https://rawgit.com/miby00/eTodo/master/eTodo/priv/www/doc/eTodo.html)
