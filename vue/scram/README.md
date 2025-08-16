# `Scram`

A [do-stuff-online](https://github.com/do-stuff-online/do-stuff-online) clone.

## Stack

* Vue.js, Vite
* TypeScript
* Shell
* Docker

## Usage

First run the below.

```console
$ docker-compose down -v && docker-compose up --build
$ cd client && npm install && npm run dev
```

Then visit the frontend at [localhost:5173](http://localhost:5173/).

## Troubleshooting

```console
$ sudo groupadd docker
$ sudo usermod -aG docker $USER
$ ls -l /var/run/docker.sock
$ sudo chown root:docker /var/run/docker.sock
```