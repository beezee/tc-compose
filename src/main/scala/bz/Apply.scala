package bz

import scala.language.higherKinds
import scalaz.Apply

object ApplyExt {

    def apply13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, R](
        a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
        a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10],
        a11: =>F[A11], a12: =>F[A12], a13: =>F[A13])(
        f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => R)(
        implicit A: Apply[F]): F[R] = {
      import A._
      ap(a13)(ap(a12)(ap(a11)(ap(a10)(
        ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(map(a1)(f.curried)))))))))))))
    }

    def apply14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, R](
        a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
        a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10],
        a11: =>F[A11], a12: =>F[A12], a13: =>F[A13], a14: =>F[A14])(
        f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => R)(
        implicit A: Apply[F]): F[R] = {
      import A._
      ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(
        ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(map(a1)(f.curried))))))))))))))
    }
}
