#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate diesel;

use std::io::Write;

use actix_http::body::Body;
use actix_http::http::header::{CONTENT_TYPE, SERVER};
use actix_http::http::{HeaderValue, StatusCode};
use actix_http::{Error, HttpService, KeepAlive, Request, Response};
use actix_server::{Server, ServerConfig};
use actix_service::{NewService, Service};
use bytes::{Bytes, BytesMut};
use futures::future::{join_all, ok, Either, FutureResult};
use futures::{Async, Future, Poll};
use serde_json::to_writer;

mod db_pg_direct;
mod models;
mod utils;

use crate::db_pg_direct::PgConnection;
use crate::utils::{FortunesTemplate, Message, Writer, SIZE};

struct App {
    dbs: Vec<PgConnection>,
    useall: bool,
    next: usize,
}

impl App {
    fn get_db(&mut self) -> &mut PgConnection {
        if self.useall {
            self.next = (self.next + 1) % 4;
            &mut self.dbs[self.next]
        } else {
            &mut self.dbs[0]
        }
    }
}

impl Service for App {
    type Request = Request;
    type Response = Response;
    type Error = Error;
    type Future = Either<
        FutureResult<Self::Response, Self::Error>,
        Box<Future<Item = Response, Error = Error>>,
    >;

    #[inline]
    fn poll_ready(&mut self) -> Poll<(), Self::Error> {
        Ok(Async::Ready(()))
    }

    fn call(&mut self, req: Request) -> Self::Future {
        let path = req.path();
        match path.len() {
            10 if path == "/plaintext" => {
                let mut res = Response::with_body(
                    StatusCode::OK,
                    Body::Bytes(Bytes::from_static(b"Hello, World!")),
                );
                res.headers_mut()
                    .insert(SERVER, HeaderValue::from_static("Actix"));
                res.headers_mut()
                    .insert(CONTENT_TYPE, HeaderValue::from_static("text/plain"));
                Either::A(ok(res))
            }
            5 if path == "/json" => {
                let message = Message {
                    message: "Hello, World!",
                };
                let mut body = BytesMut::with_capacity(SIZE);
                to_writer(Writer(&mut body), &message).unwrap();
                let mut res =
                    Response::with_body(StatusCode::OK, Body::Bytes(body.freeze()));
                res.headers_mut()
                    .insert(SERVER, HeaderValue::from_static("Actix"));
                res.headers_mut()
                    .insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
                Either::A(ok(res))
            }
            3 if path == "/db" => {
                let fut = self.dbs[0].get_world();

                Either::B(Box::new(fut.map(move |body| {
                    let mut res = Response::with_body(StatusCode::OK, Body::Bytes(body));
                    res.headers_mut()
                        .insert(SERVER, HeaderValue::from_static("Actix"));
                    res.headers_mut().insert(
                        CONTENT_TYPE,
                        HeaderValue::from_static("application/json"),
                    );
                    res
                })))
            }
            8 if path == "/fortune" => {
                let fut = self.dbs[0].tell_fortune();

                Either::B(Box::new(fut.from_err().map(move |fortunes| {
                    let mut body = BytesMut::with_capacity(2048);
                    let mut writer = Writer(&mut body);
                    let _ = write!(writer, "{}", FortunesTemplate { fortunes });
                    let mut res =
                        Response::with_body(StatusCode::OK, Body::Bytes(body.freeze()));
                    res.headers_mut()
                        .insert(SERVER, HeaderValue::from_static("Actix"));
                    res.headers_mut().insert(
                        CONTENT_TYPE,
                        HeaderValue::from_static("text/html; charset=utf-8"),
                    );
                    res
                })))
            }
            8 if path == "/queries" => {
                let q = utils::get_query_param(req.uri().query().unwrap_or("")) as usize;
                let fut = self.dbs[0].get_worlds(q);

                Either::B(Box::new(fut.from_err().map(move |worlds| {
                    let mut body = BytesMut::with_capacity(35 * worlds.len());
                    to_writer(Writer(&mut body), &worlds).unwrap();
                    let mut res =
                        Response::with_body(StatusCode::OK, Body::Bytes(body.freeze()));
                    res.headers_mut()
                        .insert(SERVER, HeaderValue::from_static("Actix"));
                    res.headers_mut().insert(
                        CONTENT_TYPE,
                        HeaderValue::from_static("application/json"),
                    );
                    res
                })))
            }
            8 if path == "/updates" => {
                let q = utils::get_query_param(req.uri().query().unwrap_or("")) as usize;
                let fut = self.dbs[0].update(q);

                Either::B(Box::new(fut.from_err().map(move |worlds| {
                    let mut body = BytesMut::with_capacity(35 * worlds.len());
                    to_writer(Writer(&mut body), &worlds).unwrap();
                    let mut res =
                        Response::with_body(StatusCode::OK, Body::Bytes(body.freeze()));
                    res.headers_mut()
                        .insert(SERVER, HeaderValue::from_static("Actix"));
                    res.headers_mut().insert(
                        CONTENT_TYPE,
                        HeaderValue::from_static("application/json"),
                    );
                    res
                })))
            }
            _ => Either::A(ok(Response::new(http::StatusCode::NOT_FOUND))),
        }
    }
}

#[derive(Clone)]
struct AppFactory;

impl NewService<ServerConfig> for AppFactory {
    type Request = Request;
    type Response = Response;
    type Error = Error;
    type Service = App;
    type InitError = ();
    type Future = Box<Future<Item = Self::Service, Error = Self::InitError>>;

    fn new_service(&self, _: &ServerConfig) -> Self::Future {
        const DB_URL: &str =
            "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

        let mut conns = Vec::new();
        for _ in 0..4 {
            conns.push(PgConnection::connect(DB_URL));
        }
        Box::new(join_all(conns).map(|dbs| App {
            dbs,
            next: 0,
            useall: num_cpus::get() > 4,
        }))
    }
}

fn main() -> std::io::Result<()> {
    let sys = actix_rt::System::builder().stop_on_panic(false).build();

    // start http server
    Server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", || {
            HttpService::build()
                .keep_alive(KeepAlive::Os)
                .h1(AppFactory)
        })?
        .start();

    println!("Started http server: 127.0.0.1:8080");
    sys.run()
}
