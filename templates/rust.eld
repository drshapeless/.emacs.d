rustic-mode

;; map_err
(me ".await.map_err(Error::SqlxError)?;")

;; debug_only
(db "if cfg!(debug_assertions) {" n n "}")

(conn "let mut conn = state.conn().await?;")

(tx "let mut tx = state.tx().await?;")

(header "let mut headers = HeaderMap::new();
headers.insert(\"HX-Trigger\", \"newTool\".parse().unwrap());")
