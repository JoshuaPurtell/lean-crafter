use crafter_web_shim::{make_router, new_app_id, shutdown_lean};
use axum::Router;
use serde::Deserialize;
use tokio::sync::oneshot;
use tokio_tungstenite::tungstenite::Message;
use futures_util::{SinkExt, StreamExt};
use std::net::SocketAddr;

#[derive(Deserialize)]
struct FrameResponse {
    id: String,
    frame: String,
}

#[derive(Deserialize)]
struct WsFrameMsg {
    #[serde(rename = "type")]
    msg_type: String,
    frame: String,
}

async fn spawn_server() -> (SocketAddr, oneshot::Sender<()>, u64) {
    let app_id = new_app_id("crafter");
    let router: Router = make_router(app_id);
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind test listener");
    listener
        .set_nonblocking(true)
        .expect("set_nonblocking");
    let addr = listener.local_addr().expect("listener addr");
    let (tx, rx) = oneshot::channel::<()>();
    let server = axum::Server::from_tcp(listener)
        .expect("server from tcp")
        .serve(router.into_make_service_with_connect_info::<SocketAddr>())
        .with_graceful_shutdown(async move {
            let _ = rx.await;
        });
    tokio::spawn(server);
    (addr, tx, app_id)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn crafter_web_smoke() {
    let (addr, shutdown_tx, app_id) = spawn_server().await;
    let base = format!("http://{}", addr);

    let client = reqwest::Client::new();
    let index = client.get(format!("{}/", base)).send().await.unwrap();
    assert!(index.status().is_success());
    let index_body = index.text().await.unwrap();
    assert!(index_body.contains("Crafter Web"));

    let css = client.get(format!("{}/styles.css", base)).send().await.unwrap();
    assert!(css.status().is_success());
    let js = client.get(format!("{}/app.js", base)).send().await.unwrap();
    assert!(js.status().is_success());

    let create = client
        .post(format!("{}/api/sessions", base))
        .send()
        .await
        .unwrap();
    assert!(create.status().is_success());
    let frame_resp: FrameResponse = create.json().await.unwrap();
    assert!(!frame_resp.id.is_empty());
    assert!(frame_resp.frame.contains("VIEW"));

    let ws_url = format!("ws://{}/api/sessions/{}/stream", addr, frame_resp.id);
    let (ws_stream, _resp) = tokio_tungstenite::connect_async(ws_url).await.unwrap();
    let (mut ws_write, mut ws_read) = ws_stream.split();

    let first = ws_read.next().await.expect("ws message").expect("ws ok");
    let first_text = match first {
        Message::Text(t) => t,
        Message::Binary(b) => String::from_utf8(b).unwrap(),
        other => panic!("unexpected ws message: {other:?}"),
    };
    let frame_msg: WsFrameMsg = serde_json::from_str(&first_text).unwrap();
    assert_eq!(frame_msg.msg_type, "frame");
    assert!(frame_msg.frame.contains("VIEW"));

    ws_write.send(Message::Text("up".to_string())).await.unwrap();
    let second = ws_read.next().await.expect("ws message").expect("ws ok");
    let second_text = match second {
        Message::Text(t) => t,
        Message::Binary(b) => String::from_utf8(b).unwrap(),
        other => panic!("unexpected ws message: {other:?}"),
    };
    let frame_msg: WsFrameMsg = serde_json::from_str(&second_text).unwrap();
    assert_eq!(frame_msg.msg_type, "frame");
    assert!(frame_msg.frame.contains("VIEW"));

    let reset = client
        .post(format!("{}/api/sessions/{}/reset", base, frame_resp.id))
        .send()
        .await
        .unwrap();
    assert!(reset.status().is_success());

    let _ = shutdown_tx.send(());
    shutdown_lean(app_id);
}
