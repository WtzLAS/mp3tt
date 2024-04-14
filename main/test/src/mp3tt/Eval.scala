package mp3tt

import utest.*
import mp3tt.model.*

object EvalTests extends TestSuite {
  val tests = Tests {
    test("eval1") {
      val tm = TmC.App(TmC.Lam("x", TmV.Refl, TmC.Return(TmV.Var(0))), TmV.Refl)
      nf(Vector.empty, tm)
    }

    test("conv1") {
      assert(conv(0, NeV.Uv(1), NeV.Uv(1)))
    }
  }
}
