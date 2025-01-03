use hyper::{Client, Request, Body};
use hyper::service::{make_service_fn, service_fn};
use std::convert::Infallible;
use std::net::SocketAddr;
use tokio_rustls::TlsAcceptor;
use tokio_rustls::rustls::{Certificate, PrivateKey};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use std::sync::{Arc};
use std::fs::File;
use std::io::{self, Write};
use std::path::Path;

#[tokio::main]
async fn main() {
    let certs = load_certs("path/to/cert.pem").expect("Failed to load certs");
    let key = load_private_key("path/to/key.pem").expect("Failed to load key");
    let config = rustls::ServerConfig::builder()
        .with_safe_defaults()
        .with_no_client_auth()
        .with_single_cert(certs, key)
        .expect("Invalid certificate or key");
    let acceptor = TlsAcceptor::from(Arc::new(config));
    let addr = SocketAddr::from(([127, 0, 0, 1], 8080));
    let listener = tokio::net::TcpListener::bind(&addr).await.unwrap();
    println!("Listening on {}", addr);
    loop {
        let (socket, _) = listener.accept().await.unwrap();
        let acceptor = acceptor.clone();
        tokio::spawn(async move {
            if let Err(e) = handle_connection(acceptor.accept(socket).await).await {
                eprintln!("Error handling connection: {:?}", e);
            }
        });
    }
}

async fn handle_connection(stream: Result<tokio_rustls::server::TlsStream<tokio::net::TcpStream>, tokio_rustls::rustls::Error>) -> Result<(), Box<dyn std::error::Error>> {
    let mut tls_stream = stream?;
    let mut buffer = vec![0; 4096]; 
    let n = tls_stream.read(&mut buffer).await?;
    let request_str = String::from_utf8_lossy(&buffer[..n]);
    export_decrypted_traffic(&request_str)?;
    let req: Request<Body> = request_str.parse()?;
    let client = Client::new();
    let res: Response<Body> = client.request(req).await?;
    let response_bytes = format!(
        "HTTP/1.1 {} {}\r\n\r\n{}",
        res.status(),
        res.version(),
        String::from_utf8_lossy(&hyper::body::to_bytes(res.into_body()).await?)
    );
    tls_stream.write_all(response_bytes.as_bytes()).await?;
    Ok(())
}

fn export_decrypted_traffic(request: &str) -> io::Result<()> {
    let mut file = File::create("decrypted_traffic.log")?;
    file.write_all(request.as_bytes())?;
    Ok(())
}

fn load_certs(path: &str) -> io::Result<Vec<Certificate>> {
    let mut certs = Vec::new();
    let cert_file = File::open(path)?;
    for line in io::BufReader::new(cert_file).lines() {
        let line = line?;
        if line.starts_with("-----BEGIN CERTIFICATE-----") {
            let mut cert_data = vec![line.clone()];
            while let Some(line) = io::BufReader::new(cert_file).lines().next() {
                let line = line?;
                cert_data.push(line);
                if line.starts_with("-----END CERTIFICATE-----") {
                    break;
                }
            }
            let cert_pem = cert_data.join("\n");
            certs.push(Certificate(cert_pem.as_bytes().to_vec()));
        }
    }
    Ok(certs)
}

fn load_private_key(path: &str) -> io::Result<PrivateKey> {
    let key_file = File::open(path)?;
    let mut key_data = String::new();
    for line in io::BufReader::new(key_file).lines() {
        key_data.push_str(&line?);
        key_data.push('\n');
        if key_data.contains("-----END PRIVATE KEY-----") {
            break;
        }
    }
    PrivateKey(key_data.as_bytes().to_vec())
}