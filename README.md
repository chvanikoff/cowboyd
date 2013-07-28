# CowboyD

## What is it all about?
CowboyD is a try to skip that part of web-app development where you embedding Cowboy.
Normally you would like to create an application, write there some code like
```erlang
start(_Type, _Args) ->
	Routes = [
		{'_', [
			{"/", handler_index, []},
			{'_', handler_notfound, []}
		]}
	],
	Nba = 100,
	Port = 8008,
	{ok, _Pid} = cowboy:start_http(http_listener, Nba, [{port, Port}], [
		{env, [{dispatch, Routes}]}
	]),
	ok.
```
Also you should include the Cowboy as a dependency and start it. And don't forget about some code reloading tool - probably you'll also need it. Not so hard but this is routine actions and CowboyD suggests you to get rid of them and focus on writing your application. You just write your application, create routes config file and some Cowboy handlers. When finished you just run CowboyD:
```bash
cowboyd start appname /path/to/appname 8008 100
```
and enjoy the results at localhost:8008

You can update your code and it'll be reloaded on the fly. routes updating is a little bit more complicated - no magic here yet and you should manually run
```bash
cowboyd routes-update appname
```

Of cause you can run multiple instances of CowboyD - multiple instances of Erlang VM will be launched (probably this part will be changed and there will be pool of running apps inside one VM).

## Installation

```bash
# cd somewhere you'd like to download the project to
cd ~/github_projects
# Clone the repo
git clone https://github.com/chvanikoff/cowboyd
# Give execution rights to cowboyd if there is no
chmod +x cowboyd/cowboyd
# Link Cowboyd to somew executable directory in your $PATH, for example /usr/bin
sudo ln -s cowboyd/cowboyd /usr/bin/cowboyd
```

## Usage:

After you've created webapp ([example](https://github.com/chvanikoff/cowboyd/tree/master/examples/webapp)) you can start/stop CowboyD and update routes for currently running applications:

```bash
cowboyd start <appname> <path> <port> [<nba>]
cowboyd stop <appname>
cowboyd routes-update <appname>
```
**appname**: *string*, is the name of your application

**path**: *string*, is a path to the root of your application

**port**: *integer*, port Cowboy will listen to

**nba**: *integer*, optional (default is 10) number of non-blocking acceptors for Cowboy