# eTodo
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

## Installation

eTodo require an Erlang installation with a working wxErlang environment. 

I recommend using Erlang Solutions [installation packages](https://www.erlang-solutions.com/downloads/download-erlang-otp).

### Download and run eTodo

1. Install Erlang
2. Download eTodo from github using the "download.ZIP" button
3. Extract the files
4. Go into the folder eTodo/priv
5. On Unix/Mac: run StartETodo.sh. On Windows: run StartETodo.bat

Have patience: The first startup needs to create an empty TODO database.

[Getting started with eTodo](https://rawgit.com/miby00/eTodo/master/eTodo/priv/www/doc/eTodo.html)

### Making an eTodo release which includes Erlang

To make it easier to use eTodo for non Erlang fanatics I recommend building an eTodo release package.
When you have built this package it includes Erlang, so the users of the package doesn't need Erlang to run eTodo.

The release package will work on the platform on which it is built. So for instance a Windows 64-bit release will work on a Windows 64-bit machine.

Howto make an eTodo release package:

1. Go into the folder eTodo/release
2. On Unix/Mac: run BuildRelease.sh. On Windows: run BuildRelease.bat

This creates an eTodo.zip, eTodo.tar.gz and an eTodo directory. All three contain the release.

To install eTodo from a release: Just extract the files in the eTodo release package on the target computer.

To run eTodo from a release: Enter the folder eTodo/bin, run erl.

###Status

eTodo isn't a finished, when I feel inspired I code on it during my spare time.
So do not expect it to be perfect, commented or fully documented ;)


