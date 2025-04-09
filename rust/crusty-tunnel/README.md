# `Crusty Tunnel` 🚇

Naive HTTPS proxy server that intercepts and logs decrypted HTTPS traffic, written in Rust. 

## Usage

1. Run this code.

```console
$ git clone https://github.com/gongahkia/crusty-tunnel
$ cd crab_proxy
```

2. Modify the `src/main.rs` file to specify the paths to your certificate and key files.

```rs
let certs = load_certs("path/to/cert.pem").expect("Failed to load certs");
let key = load_private_key("path/to/key.pem").expect("Failed to load key");
```

3. Run this code.

```console
$ cargo run --release
```

4. Visit [`http://127.0.0.1:8080`](http://127.0.0.1:8080), decrypted traffic is logged to `decrypted_traffic.log`.