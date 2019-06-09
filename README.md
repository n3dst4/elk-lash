# **elk-lash**: a multi-word anagram and subgram generator

![Build status](https://travis-ci.org/n3dst4/elk-lash.svg?branch=master)

```txt
$ stack exec anagram-cli haskell
Finished reading dictionary (50545)
elk lash
hell ask
ask hell
lash elk
```

This is a mucking about project for me to try a bit of Haskell on.

## Building and running

This project was made using [Stack][stack]. Sorry, no idea how to build it with
anything else. Also it's set up to build through Docker to get repeatable builds
that are guaranteed to work inside the Docker images that are produced.

1. Install [Docker](https://docs.docker.com/install/).
2. Give yourself non-sudo access to docker:

    ```sh
    sudo usermod -aG docker $USER
    ```

3. Log out & in so the above can kick in.
4. Install [Stack][stack].
5. Clone this repo & `cd` into it.
6. Build the base image: 

    ```sh
    ./baseimage/build.sh
    ```

7. Setup slack stuff in the project:

    ```sh
    stack setup
    stack docker pull
    ```

8. Run the actual, build:

    ```sh
    stack build
    ```

## Run the command line tool

This tool can do subgrams or anagrams. Anagrams is the default, but the the
performance is `On^n` or something equally terrifying to caveat emptor yeah?

```bash
$ stack exec anagram-cli "Chunky monkey\!"
Finished reading dictionary (50545)
on my key chunk
on my chunk key
on key my chunk
on key chunk my
on chunk my key
on chunk key my
no my key chunk
no my chunk key
no key my chunk
no key chunk my
no chunk my key
no chunk key my
nun my key hock
...etc...
```

To do subgrams, give it the `-s` flag and alos you need to use the `--` form
with `slack exec`, like this:

```sh
$ stack exec anagram-cli -- "Chunky monkey\!" -s
Finished reading dictionary (50545)
you
on
no
nun
non
noun
my
mu
monk
oh
ho
hum
ohm
homy
hymn
hunk
honk
...etc...
```

## Run the API

You can run the API via slack:

```sh
$ stack exec anagram-service
Finished reading dictionary (50545)
Setting phasers to stun... (port 3000) (ctrl-c to quit)
```

But for production (hahahaha) use it's intended to be run as a container.

## Docker-based build

Stack provides a system for performing builds inside a docker container. This is
good because it gives you repeatable, reliable builds that don't depend on the
developer's personal workstation configuration.

Slack also, and separately, has a system for building Docker images of your
built software.

The second one works best if you use the first one.

To get the Docker-based build working you need to run

```sh
stack docker pull
```

It used to be that if you were using Docker builds, `stack build` would pull the
necessary images for you but it seems like that's no longer working.

To build the docker image of the software, you need to (once) build the base
image:

```sh
./baseimage/build.sh
```

This is because when you use Stack to build your image, there's no way to
configure and custom steps, so you have to use a custom base image.

Anyway once that's done,

```sh
stack docker image
```

Builds the image.

## Optimization

Previous versions of the algorithm used a memoized internal function but actual
benchmarking revealed that it hurting performance by about 30%.

[stack]: https://docs.haskellstack.org/en/stable/README/
[docker]: https://docs.docker.com/install/
