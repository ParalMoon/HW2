module F = Format

let rec interp_expr (e: Ast.expr) (g: FStore.t) (s: Store.t) : Value.t = 
  match e with
  | Ast.Num n ->
      Value.NumV n

  | Ast.Id x ->
      if Store.mem x s then
        Store.find x s
      else
        raise (Failure ("Free identifier: " ^ x))

  | Ast.Add (e1, e2) ->
      let Value.NumV n1 = interp_expr e1 g s in
      let Value.NumV n2 = interp_expr e2 g s in
      Value.NumV (n1 + n2)

  | Ast.Sub (e1, e2) ->
      let Value.NumV n1 = interp_expr e1 g s in
      let Value.NumV n2 = interp_expr e2 g s in
      Value.NumV (n1 - n2)

  | Ast.LetIn (x, e1, e2) ->
      (* let x = e1 in e2 *)
      let v1 = interp_expr e1 g s in
      let s' = Store.add x v1 s in
      interp_expr e2 g s'

  | Ast.Call (fname, args) ->
    (* function lookup *)
    if not (FStore.mem fname g) then
      raise (Failure ("Undefined function: " ^ fname))
    else
      let (params, body) = FStore.find fname g in

      (* 먼저 파라미터 개수와 인자 개수 비교 *)
      let required = List.length params in
      let actual = List.length args in
      if required <> actual then
        raise (Failure ("The number of arguments of " ^ fname ^
                        " mismatched: Required: " ^
                        string_of_int required ^
                        ", Actual: " ^ string_of_int actual));

      (* 인자 평가 (call-by-value strict) *)
      let arg_vals = List.map (fun a -> interp_expr a g s) args in

      (* σ' 생성: fresh store *)
      let rec bind store ps vs =
        match ps, vs with
        | [], [] -> store
        | p::ps2, v::vs2 -> bind (Store.add p v store) ps2 vs2
        | _ -> failwith "unreachable"
      in
      let s' = bind Store.empty params arg_vals in

      (* 함수 본문 평가 *)
      interp_expr body g s'

let interp_fundef (d: Ast.fundef) (g: FStore.t) : FStore.t = 
  match d with
  | Ast.FunDef (fname, params, body) ->
      (* Λ[fname ↦ (params, body)] *)
      FStore.add fname (params, body) g


let interp (p: Ast.prog) : Value.t = 
  match p with
  | Ast.Prog (defs, expr) ->
      (* 1. def 리스트를 모두 읽어서 함수 환경 Λ 만들기 *)
      let g =
        List.fold_left
          (fun acc defn -> interp_fundef defn acc)
          FStore.empty
          defs
      in
      (* 2. 변수 환경은 빈 σ에서 시작 *)
      let s = Store.empty in
      (* 3. 본문 expression을 (Λ, σ)에서 평가 *)
      interp_expr expr g s








