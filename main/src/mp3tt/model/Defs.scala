package mp3tt.model

type Idx = Int

type Lvl = Int

extension (x: Lvl) def toIdx(l: Lvl): Idx = l - x - 1

type Name = String

enum Raw {
  // --- Values ---
  // Uv i
  case Uv(i: Int)
  // U X
  case U(x: Raw)
  // Σ (x : A). B
  case Sigma(x: Name, a: Raw, b: Raw)
  // A + B
  case Sum(a: Raw, b: Raw)
  // v ==_A w
  case Eq(a: Raw, v: Raw, w: Raw)
  // x
  case Var(x: Name)
  // thunk t
  case Thunk(t: Raw)
  // (v, w)
  case Pair(v: Raw, w: Raw)
  // inl v
  case Inl(v: Raw)
  // inr w
  case Inr(w: Raw)
  // refl
  case Refl

  // --- Computations ---
  // Uc i
  case Uc(i: Int)
  // F A
  case F(a: Raw)
  // Π (x : A). B
  case Pi(x: Name, a: Raw, b: Raw)
  // force t
  case Force(t: Raw)
  // λ(x : A). t
  case Lam(x: Name, a: Raw, t: Raw)
  // t v
  case App(t: Raw, v: Raw)
  // let x : A = t in u
  case Let(x: Name, a: Raw, t: Raw, u: Raw)
  // return v
  case Return(v: Raw)
  // recSigma v X t
  case RecSigma(v: Raw, x: Raw, t: Raw)
  // recSum v X t1 t2
  case RecSum(v: Raw, x: Raw, t1: Raw, t2: Raw)
  // recEq v X t
  case RecEq(v: Raw, x: Raw, t: Raw)
}

type Ty = TyV | TyC
type Tm = TmV | TmC

type TyV = TmV
type TyC = TmC

enum TmV {
  case Uv(i: Int)

  case Sigma(x: Name, a: TyV, b: TyV)
  case Pair(v: TmV, w: TmV)

  case Sum(a: TyV, b: TyV)
  case Inl(v: TmV)
  case Inr(w: TmV)

  case Eq(a: TyV, v: TmV, w: TmV)
  case Refl

  case U(x: TyC)
  case Thunk(t: TmC)

  case Var(x: Idx)
}

enum TmC {
  case Uc(i: Int)

  case Pi(x: Name, a: TyV, b: TyC)
  case Lam(x: Name, a: TyV, t: TmC)
  case App(t: TmC, v: TmV)

  case F(a: TyV)
  case Return(v: TmV)

  case Force(a: TyV)

  case Let(x: Name, a: TyV, t: TmC, u: TmC)
  case DLet(x: Name, a: TyV, t: TmC, u: TmC)

  case RecSigma(v: TmV, x: TyC, t: TmC)
  case RecSum(v: TmV, x: TyC, t1: TmC, t2: TmC)
  case RecEq(v: TmV, x: TyC, t: TmC)
}

type Env = Vector[NeV]

case class Closure[T](env: Env, t: T)

type NeTy = NeTyV | NeTyC
type Ne = NeV | NeC

type NeTyV = NeV
type NeTyC = NeC

enum NeV {
  case Uv(i: Int)

  case Sigma(x: Name, a: NeV, b: Closure[TmV])
  case Pair(v: NeV, w: NeV)

  case Sum(a: NeTyV, b: NeTyV)
  case Inl(v: NeV)
  case Inr(w: NeV)

  case Eq(a: NeTyV, v: NeV, w: NeV)
  case Refl

  case U(x: NeTyC)
  case Thunk(t: NeC)

  case Var(x: Lvl)
}

enum NeC {
  case Uc(i: Int)

  case Pi(x: Name, a: NeTyV, b: Closure[TmC])
  case Lam(x: Name, a: NeTyV, t: Closure[TmC])
  case App(t: NeC, v: NeV)

  case F(a: NeTyV)
  case Return(v: NeV)

  case Force(a: NeTyV)

  case Let(x: Name, a: NeTyV, t: NeC, u: Closure[TmC])
  case DLet(x: Name, a: NeTyV, t: NeC, u: Closure[TmC])

  case RecSigma(v: NeV, x: NeTyC, t: NeC)
  case RecSum(v: NeV, x: NeTyC, t1: NeC, t2: NeC)
  case RecEq(v: NeV, x: NeTyC, t: NeC)
}

case class Ctx(env: Env, types: Vector[(Name, NeTyV)], l: Lvl) {
  def bind(x: Name, a: NeTyV) =
    Ctx(NeV.Var(l) +: env, (x, a) +: types, l + 1)
  def define(x: Name, t: NeV, a: NeTyV) =
    Ctx(t +: env, (x, a) +: types, l + 1)
  def find() = {}
}

object Ctx {
  def empty = Ctx(Vector.empty, Vector.empty, 0)
}

type M[A] = Either[TyCkErr, A]
