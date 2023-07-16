rustic-mode

;; map_err
(me ".await.map_err(Error::SqlxError)?;")

;; debug_only
(db "if cfg!(debug_assertions) {" n n "}")
