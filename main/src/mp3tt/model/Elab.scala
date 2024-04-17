package mp3tt.model

import cats.syntax.all.*

def checkV(ctx: Ctx, raw: Raw, a: NeTyV): M[TmV] = raw match
  case Raw.Uv(i)          => ???
  case Raw.U(x)           => ???
  case Raw.Sigma(x, a, b) => ???
  case Raw.Sum(a, b)      => ???
  case Raw.Eq(a, v, w)    => ???
  case Raw.Var(x)         => ???
  case Raw.Thunk(t)       => ???
  case Raw.Pair(v, w)     => ???
  case Raw.Inl(v)         => ???
  case Raw.Inr(w)         => ???
  case Raw.Refl           => ???
  case _                  => ???

def checkC(ctx: Ctx, raw: Raw, a: NeTyC): M[TmC] = ???

def checkUc(ctx: Ctx, raw: Raw): M[(TmC, NeC.Uc)] = for {
  (tm, ty) <- inferC(ctx, raw)
  res <- ty match
    case p: NeC.Uc => p.pure[M]
    case _         => TypeMismatch(s"$ty", "NeC.Uc(i)").raiseError[M, NeC.Uc]
} yield (tm, res)

def checkUv(ctx: Ctx, raw: Raw): M[(TmV, NeV.Uv)] = for {
  (tm, ty) <- inferV(ctx, raw)
  res <- ty match
    case p: NeV.Uv => p.pure[M]
    case _         => TypeMismatch(s"$ty", "NeV.Uv(i)").raiseError[M, NeV.Uv]
} yield (tm, res)

def inferV(ctx: Ctx, raw: Raw): M[(TmV, NeTyV)] = raw match
  case Raw.Uv(i) => (TmV.Uv(i), NeV.Uv(i + 1)).pure
  case Raw.U(x) =>
    for {
      (tm, ty) <- checkUc(ctx, x)
    } yield (TmV.U(tm), NeV.Uv(ty.i))
  case Raw.Sigma(x, a, b) =>
    for {
      (atm, aty) <- checkUv(ctx, a)
      (btm, bty) <- checkUv(ctx.bind(x, eval(ctx.env, atm)), b)
    } yield (TmV.Sigma(x, atm, btm), NeV.Uv(math.max(aty.i, bty.i)))
  case Raw.Sum(a, b) =>
    for {
      (atm, aty) <- checkUv(ctx, a)
      (btm, bty) <- checkUv(ctx, b)
    } yield (TmV.Sum(atm, btm), NeV.Uv(math.max(aty.i, bty.i)))
  case Raw.Eq(a, v, w) =>
    for {
      (atm, aty) <- checkUv(ctx, a)
      ane <- eval(ctx.env, atm).pure[M]
      vtm <- checkV(ctx, v, ane)
      wtm <- checkV(ctx, w, ane)
    } yield (TmV.Eq(atm, vtm, wtm), aty)
  case Raw.Var(x)      => {
    (???, ???).pure
  }
  case Raw.Thunk(t)    => ???
  case Raw.Pair(v, w)  => ???
  case Raw.Inl(v)      => ???
  case Raw.Inr(w)      => ???
  case Raw.Refl        => ???
  case _               => ???

def inferC(ctx: Ctx, raw: Raw): M[(TmC, NeTyC)] = ???
