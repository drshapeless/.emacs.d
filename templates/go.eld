go-mode
go-ts-mode

;; Cancel context
(ctx
 > "ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)" n
 > "defer cancel()" n)

(en
 > "if err != nil {" n p q n "}" n)

(js "`json:" p "`")

(main "package main" n n
      "func main() {" n
      > p n
      "}" n)

(wr "func (app *Application) " p "(w http.ResponseWriter, r *http.Request) {" n
    > p n
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
