# Trying out [Deno](https://deno.com/)

An alternative to traditional JS/TS runtimes like Node.js.

## Usage

Add the below lines to your `.bashrc`.

```sh
export DENO_INSTALL="$HOME/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"
```

Then run.

```console
$ sudo apt-get update -y && sudo apt-get upgrade -y
$ sudo apt install -y unzip
$ curl -fsSL https://deno.land/x/install/install.sh | sh
$ deno run --allow-net server.ts
```

## Why use Deno?

1. **Security**:
    * Deno has NO access to the local file system, network, or environment variables unless explicitly granted by the developer, *reducing the attack surface* available for malicious actors to latch onto. 
    * Any access must be EXPLICITLY defined via permissions by the Developer. 
    * Sandboxing of all runtime code allows for greater security since even executed malicious code requires requesting for further permissions before code execution.
2. **Simplicity**: 
    * Designed to "just work" out of the box with minimal configuration required for JS/TS projects.
3. **Modern development practices**: 
    * ES modules are used as the default module system, and modules are declaratively imported directly via URLs as opposed to a centralized package manager like npm.
4. **Typescript integration**: 
    * First-class TS support and type-checking for JS files by default.