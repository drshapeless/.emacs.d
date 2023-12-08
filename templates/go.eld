go-mode
go-ts-mode

;; Cancel context
(ctx
 > "ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)" n
 > "defer cancel()" n)

(en
 > "if err != nil {" n > p q n "\t}" n)

(ef
 > "if err != nil {" n
 > "app.serverError(err, w)" n
 > "return" n "\t}")

(er
 > "if err != nil {" n
 > "return err" n
 > "}")

(db "db *pgxpool.Pool")

(js "`json:" p "`")

(main "package main" n n
      "func main() {" n
      > p n
      "}" n)

(wr "func (app *application) " p q "(w http.ResponseWriter, r *http.Request) {" n
    > n
    "}" n)

(swag "// " p n
      "// @Summary " p n
      "// @Description "p n
      "// @Tags " p n
      "// @Accept  json" n
      "// @Produce  json" n
      "// @Param Authorization header string true \"Bearer\"" n
      "// @Param data body swag_user_input true \"The input user struct\"" n
      "// @Success 200 {}" n
      "// @Failure 400 {}" n
      "// @Router / [get]" n)

(snf
 > "switch {" n
 > "case errors.Is(err, data.ErrRecordNotFound):" n
 > "app.notFoundResponse(w, r)" n
 > "default:" n
 > "app.serverErrorResponse(w, r, err)" n
 > "}" n
 > "return" n
)

(test "func Test" p "(t *testing.T) {" n
      > p n
      "}" n)

(tx > "tx, err := app.db.Begin(ctx)" n
    > "if err != nil {" n
    > "app.serverError(err, w)" n
    > "return" n
     "\t}" n
    > "defer tx.Rollback(ctx)" n
    > p q n
    > "err = tx.Commit(ctx)" n
    > "if err != nil {" n
        > "app.serverError(err, w)" n
    > "return" n
     "\t}" n
)

(rows > "rows, err := db.Query(ctx, q, " p q ")" n
      > "if err != nil {" n
      > "return nil, err" n
      > "}" n)
