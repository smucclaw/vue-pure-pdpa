# Dolores - Frontend Expert System

This is a demo of a frontend expert system with the Personal Data Protection Act (PDPA) example.

## Requirements
- NodeJS (versions 16 and above)

## Setup and Usage

### Configuring Environment Variables with `.env`

Before proceeding with the steps below, copy the `.env.example` to `.env`. Inside the `.env` file, configure the following settings according to your application:

```
BASE_URL=/
VUE_APP_BROWSER_NAME=CCLAW
VUE_APP_NAME="PDPA DBNO PoC - Draft"
```

### Installing NVM

NVM is a version manager for node. Follow the instructions on the [nvm github](https://github.com/nvm-sh/nvm).

### Installing Packages

First, install nvm from https://nvm.sh/

Then run the following set of install scripts, from inside the vue-pure-pdpa directory:

```shell
$ nvm install
$ nvm use
$ nvm exec
$ npm install -g node-gyp@latest
$ npm install
$ npm run deps
```

### Running in Development

Start the application in development mode by running:

```shell
$ npm run serve
```

By default, that brings up the application at `localhost:8080`.

If you've been assigned a different port 8xxx, use

```shell
$ npm run serve -- --port=8xxx
```

If you have a working web app at the interface, you should be able to proceed to use this repo as part of the L4 backend.

### Building for Production

To build the application for production, run:

```shell
$ npm run build
```

A `dist/` directory should be produced. Deploy the contents in the `dist/` directory to your application of choice (e.g. Github pages).

An example can be found at <https://smucclaw.github.io/vue-pure-pdpa/> or <https://smucclaw.github.io/mengwong/pdpa/>.

Basically, mv the relevant files from `dist/` into the `smucclaw/smucclaw.github.io` repository.

### Using Docker

Alternatively, you may run this application on a Docker container by running:

```shell
$ docker-compose up --build
``` 

### Using v8k

v8k is a little server-management script that allocates new development servers to ports in the 8000 range.

Once you're able to run `npm run serve` you can think about spawning new servers in response to updates from the Python/Flask/natural4-exe end of things.

#### Setup

You only have to do this once:

```
mkdir ~/v8kworkdir
cd ~/v8kworkdir
export V8K_WORKDIR=$HOME/v8kworkdir
git clone git://github.com/smucclaw/vue-pure-pdpa
rsync -va vue-pure-pdpa/ vue-big/
(cd vue-big; npm i; npm run deps)
rsync -va --exclude={.spago,.git,node_modules} vue-big/ vue-small/
cd vue-small
ln -s ../vue-big/.git .
ln -s ../vue-big/node_modules .
ln -s ../vue-big/.spago .
```

Remember to update your gunicorn conf file to set V8K_WORKDIR to the path as above.

This sets you up with `vue-small`, which is a copy of `vue-big`, which is a copy of the `vue-pure-pdpa` repo.

`vue-small` takes up a bit less space, by taking advantage of symlinks to reuse existing files that don't change across copies.

This becomes valuable because `v8k` later rsyncs `vue-small` to `vue-01`, `vue-02`, and so on, at runtime. This rsync happens when we're trying to hurry: the end-user could click on the "vue web app" link at any time, so we want to bring up the vue web app as fast as possible.

If the `vue-YY` directory doesn't already exist, then the rsync is liable to be slow, so we want to minimize the size of the `vue-small`.

We could pre-rsync the `vue-YY` directories at this point, too, with something like

```
for yy in 01 02 03 04 05 06 07 08 09; do rsync -va vue-small/ vue-$yy/; done
```

#### Spawning a new server

Every time the Python Flask subsystem runs natural4-exe to refresh the `workdir` output, it will want to tell Vue "here is a new .purs file that contains a Rule Library".

It should run:

```
~/src/smucclaw/vue-pure-pdpa/bin/v8k up --uuid=000 --ssid=111 --sheetid=222 ~/src/smucclaw/dsl/lib/haskell/natural4/workdir/000/111/222/purs/LATEST.purs
```

The STDOUT of this command will be a port number and path. Append that to the server public ip or domain name to get something like

http://18.139.62.80:8001/000/111/222/

And a few seconds later that link should serve an instance of the Vue
app configured with the `LATEST.purs` that you gave it.

The working name for this expert-system part of the web app is called
Orwell, because it is good at evaluating And/Or trees in the context
of a mechanized public service interface with the public that computes
truth values.

The above `up` call is idempotent: the Flask system can just re-run it
when the `workdir` changes and serve the constructed URL back to the
sidebar. Most of the time the URL will not change but sometimes it
will, so do read it each time.

#### Bringing Down A Server

Usually you can just leave the servers running. The v8k script has the
notion of a pool, whose default size is 10; after 10 slots are
allocated, new `up` commands will overwrite the oldest server.

If you have the notion of "ending a session" in the Google Sheets
"IDE" frontend, the Python Flask subsystem is invited to call

```
~/src/smucclaw/vue-pure-pdpa/bin/v8k down --uuid=000 --ssid=111 --sheetid=222
```

This will deallocate the running instance and make the slot available.

#### Bringing Down A Server With Great Prejudice

If you know for sure you want to bring down slot 03,

```
~/src/smucclaw/vue-pure-pdpa/bin/v8k downdir 03
```

#### Checking to see what's running using v8k

This command will walk the V8K_WORKDIR to see what's going on -- in theory.

```
~/src/smucclaw/vue-pure-pdpa/bin/v8k list
```

#### Checking to see what's running

If you don't trust it, probe the OS:

```
ps wwaux | grep port=80
```

to see what's on port 80something.


