package mp3tt.model

extension (c: Closure[TmV]) infix def $$(v: NeV) = eval(v +: c.env, c.t)
extension (c: Closure[TmC]) infix def $$(v: NeV) = eval(v +: c.env, c.t)

def eval(env: Env, term: TmV): NeV = term match
  case TmV.Uv(i)          => NeV.Uv(i)
  case TmV.Sigma(x, a, b) => NeV.Sigma(x, eval(env, a), Closure[TmV](env, b))
  case TmV.Pair(v, w)     => NeV.Pair(eval(env, v), eval(env, w))
  case TmV.Sum(a, b)      => NeV.Sum(eval(env, a), eval(env, b))
  case TmV.Inl(v)         => NeV.Inl(eval(env, v))
  case TmV.Inr(w)         => NeV.Inr(eval(env, w))
  case TmV.Eq(a, v, w)    => NeV.Eq(eval(env, a), eval(env, v), eval(env, w))
  case TmV.Refl           => NeV.Refl
  case TmV.U(x)           => NeV.U(eval(env, x))
  case TmV.Thunk(t)       => NeV.Thunk(eval(env, t))
  case TmV.Var(x)         => env(x)

def eval(env: Env, term: TmC): NeC = term match
  case TmC.Uc(i)        => NeC.Uc(i)
  case TmC.Pi(x, a, b)  => NeC.Pi(x, eval(env, a), Closure[TmC](env, b))
  case TmC.Lam(x, a, t) => NeC.Lam(x, eval(env, a), Closure[TmC](env, t))
  case TmC.App(t, v) =>
    (eval(env, t), eval(env, v)) match
      case (NeC.Lam(_, _, t), v) => t $$ v
      case (t, v)                => NeC.App(t, v)
  case TmC.F(a)      => NeC.F(eval(env, a))
  case TmC.Return(v) => NeC.Return(eval(env, v))
  case TmC.Force(a) =>
    eval(env, a) match
      case NeV.Thunk(t) => t
      case a            => NeC.Force(a)
  case TmC.Let(x, a, t, u) =>
    eval(env, t) match
      case NeC.Return(v) => eval(v +: env, u)
      case _ => NeC.Let(x, eval(env, a), eval(env, t), Closure[TmC](env, u))
  case TmC.DLet(x, a, t, u) =>
    eval(env, t) match
      case NeC.Return(v) => eval(v +: env, u)
      case t             => NeC.DLet(x, eval(env, a), t, Closure[TmC](env, u))
  case TmC.RecSigma(v, x, t) =>
    eval(env, v) match
      case NeV.Pair(v, w) =>
        eval(
          env,
          TmC.App(TmC.App(t, quote(env.length, v)), quote(env.length, w))
        )
      case v => NeC.RecSigma(v, eval(env, x), eval(env, t))
  case TmC.RecSum(v, x, t1, t2) =>
    eval(env, v) match
      case NeV.Inl(v) => eval(env, TmC.App(t1, quote(env.length, v)))
      case NeV.Inr(w) => eval(env, TmC.App(t2, quote(env.length, w)))
      case v => NeC.RecSum(v, eval(env, x), eval(env, t1), eval(env, t2))
  case TmC.RecEq(v, x, t) =>
    eval(env, v) match
      case NeV.Refl => eval(env, t)
      case v        => NeC.RecEq(v, eval(env, x), eval(env, t))

def quote(l: Lvl, v: NeV): TmV = v match
  case NeV.Uv(i) => TmV.Uv(i)
  case NeV.Sigma(x, a, b) =>
    TmV.Sigma(x, quote(l, a), quote(l + 1, b $$ NeV.Var(l)))
  case NeV.Pair(v, w)  => TmV.Pair(quote(l, v), quote(l, w))
  case NeV.Sum(a, b)   => TmV.Sum(quote(l, a), quote(l, b))
  case NeV.Inl(v)      => TmV.Inl(quote(l, v))
  case NeV.Inr(w)      => TmV.Inr(quote(l, w))
  case NeV.Eq(a, v, w) => TmV.Eq(quote(l, a), quote(l, v), quote(l, w))
  case NeV.Refl        => TmV.Refl
  case NeV.U(x)        => TmV.U(quote(l, x))
  case NeV.Thunk(t)    => TmV.Thunk(quote(l, t))
  case NeV.Var(x)      => TmV.Var(x.toIdx(l))

def quote(l: Lvl, c: NeC): TmC = c match
  case NeC.Uc(i) => TmC.Uc(i)
  case NeC.Pi(x, a, b) =>
    TmC.Pi(x, quote(l, a), quote(l + 1, b $$ NeV.Var(l)))
  case NeC.Lam(x, a, t) =>
    TmC.Lam(x, quote(l, a), quote(l + 1, t $$ NeV.Var(l)))
  case NeC.App(t, v) => TmC.App(quote(l, t), quote(l, v))
  case NeC.F(a)      => TmC.F(quote(l, a))
  case NeC.Return(v) => TmC.Return(quote(l, v))
  case NeC.Force(a)  => TmC.Force(quote(l, a))
  case NeC.Let(x, a, t, u) =>
    TmC.Let(x, quote(l, a), quote(l, t), quote(l + 1, u $$ NeV.Var(l)))
  case NeC.DLet(x, a, t, u) =>
    TmC.DLet(x, quote(l, a), quote(l, t), quote(l + 1, u $$ NeV.Var(l)))
  case NeC.RecSigma(v, x, t) =>
    TmC.RecSigma(quote(l, v), quote(l, x), quote(l, t))
  case NeC.RecSum(v, x, t1, t2) =>
    TmC.RecSum(quote(l, v), quote(l, x), quote(l, t1), quote(l, t2))
  case NeC.RecEq(v, x, t) => TmC.RecEq(quote(l, v), quote(l, x), quote(l, t))

def nf(env: Env, t: TmV): TmV = quote(env.length, eval(env, t))

def nf(env: Env, t: TmC): TmC = quote(env.length, eval(env, t))

def conv(l: Lvl, t: Ne, u: Ne): Boolean = (t, u) match
  case (NeV.Uv(it), NeV.Uv(iu)) => it == iu
  case (NeV.Sigma(_, at, bt), NeV.Sigma(_, au, bu)) =>
    conv(l, at, au) && conv(l + 1, bt $$ NeV.Var(l), bu $$ NeV.Var(l))
  case (NeV.Pair(vt, wt), NeV.Pair(vu, wu)) =>
    conv(l, vt, vu) && conv(l, wt, wu)
  case (NeV.Sum(at, bt), NeV.Sum(au, bu)) =>
    conv(l, at, au) && conv(l, bt, bu)
  case (NeV.Inl(vt), NeV.Inl(vu)) => conv(l, vt, vu)
  case (NeV.Inr(wt), NeV.Inr(wu)) => conv(l, wt, wu)
  case (NeV.Eq(at, vt, wt), NeV.Eq(au, vu, wu)) =>
    conv(l, at, au) && conv(l, vt, vu) && conv(l, wt, wu)
  case (NeV.Refl, NeV.Refl)           => true
  case (NeV.U(xt), NeV.U(xu))         => conv(l, xt, xu)
  case (NeV.Thunk(tt), NeV.Thunk(tu)) => conv(l, tt, tu)
  case (NeV.Var(xt), NeV.Var(xu))     => xt == xu

  case (NeC.Uc(it), NeC.Uc(iu)) => it == iu
  case (NeC.Pi(_, at, bt), NeC.Pi(_, au, bu)) =>
    conv(l, at, au) && conv(l + 1, bt $$ NeV.Var(l), bu $$ NeV.Var(l))
  case (NeC.Lam(_, at, tt), NeC.Lam(_, au, tu)) =>
    conv(l, at, au) && conv(l + 1, tt $$ NeV.Var(l), tu $$ NeV.Var(l))
  case (NeC.App(tt, vt), NeC.App(tu, vu)) =>
    conv(l, tt, tu) && conv(l, vt, vu)
  case (NeC.F(at), NeC.F(au))           => conv(l, at, au)
  case (NeC.Return(vt), NeC.Return(vu)) => conv(l, vt, vu)
  case (NeC.Force(at), NeC.Force(au))   => conv(l, at, au)
  case (NeC.Let(_, at, tt, ut), NeC.Let(_, au, tu, uu)) =>
    conv(l, at, au) && conv(l, tt, tu) && conv(
      l + 1,
      ut $$ NeV.Var(l),
      uu $$ NeV.Var(l)
    )
  case (NeC.DLet(_, at, tt, ut), NeC.DLet(_, au, tu, uu)) =>
    conv(l, at, au) && conv(l, tt, tu) && conv(
      l + 1,
      ut $$ NeV.Var(l),
      uu $$ NeV.Var(l)
    )
  case (NeC.RecSigma(vt, xt, tt), NeC.RecSigma(vu, xu, tu)) =>
    conv(l, vt, vu) && conv(l, xt, xu) && conv(l, tt, tu)
  case _ => false
