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

* [Mac 64-bit](https://cdn.rawgit.com/miby00/eTodo-releases/v1.0.0-beta/Mac/eTodo.dmg)
* [Windows 32-bit](https://cdn.rawgit.com/miby00/eTodo-releases/v1.0.0-beta/Windows 7/eTodo.exe)
* [Ubuntu 64-bit](https://cdn.rawgit.com/miby00/eTodo-releases/v1.0.0-beta/Ubuntu/etodo_1.0.0_amd64.deb)

If no prebuilt package is available for your platform you can download and run eTodo according to 
the instructions below.

## Installation

eTodo require an Erlang installation with a working wxErlang environment. 

I recommend using Erlang Solutions [installation packages](https://www.erlang-solutions.com/downloads/download-erlang-otp).

### Download, build and run eTodo

Feel free to ignore steps already taken ;-)

* Install [Erlang](https://www.erlang-solutions.com/downloads/download-erlang-otp)
* Install [rebar3](https://github.com/erlang/rebar3)
* Download code using **"download.ZIP"** button or use:

        $ git clone https://github.com/miby00/eTodo.git
        
Before you run eTodo, enter the config folder and edit the *sys.config* file.
Set the *dir* and *logDir* configurations.

  Do one of the following (in the root folder of the project):
  * Make a release using the command:
  
        $ rebar3 release
  * Run a rebar shell using the command:
   
        $ rebar3 shell
        
If you made a release the release can be found in the folder:

    ./_build/default/rel/eTodo
    
To run the release enter the bin folder of the release and use the command:

    ./eTodo
    
Have patience: The first startup needs to create an empty TODO database.

[Getting started with eTodo](https://rawgit.com/miby00/eTodo/master/priv/www/docs/eTodo.html)

###Status

eTodo isn't finished, when I feel inspired I code on it during my spare time.
So do not expect it to be perfect, commented, have test cases or be fully documented ;)

###Contributors

I would like to give special thanks to:

* [Anders Ramsell](https://github.com/andersramsell)
* [Knut Sveidqvist](https://github.com/knsv)
* [Gunnar Sverredal](https://github.com/donGunnar)

For contributing to the project.
