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
    def apply15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, R](
        a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
        a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10],
        a11: =>F[A11], a12: =>F[A12], a13: =>F[A13], a14: =>F[A14], a15: =>F[A15])(
        f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => R)(
        implicit A: Apply[F]): F[R] = {
      import A._
      ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(
        ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(map(a1)(f.curried)))))))))))))))
    }
    def apply16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
                A15, A16, R](
        a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
        a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10],
        a11: =>F[A11], a12: =>F[A12], a13: =>F[A13], a14: =>F[A14], a15: =>F[A15],
        a16: =>F[A16])(
        f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => R)(
        implicit A: Apply[F]): F[R] = {
      import A._
      ap(a16)(ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(
        ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(map(a1)(f.curried))))))))))))))))
    }
    def apply17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
                A15, A16, A17, R](
        a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
        a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10],
        a11: =>F[A11], a12: =>F[A12], a13: =>F[A13], a14: =>F[A14], a15: =>F[A15],
        a16: =>F[A16], a17: =>F[A17])(
        f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => R)(
        implicit A: Apply[F]): F[R] = {
      import A._
      ap(a17)(ap(a16)(ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(
        ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(map(a1)(f.curried)))))))))))))))))
    }
    def apply18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
                A15, A16, A17, A18, R](
        a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
        a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10],
        a11: =>F[A11], a12: =>F[A12], a13: =>F[A13], a14: =>F[A14], a15: =>F[A15],
        a16: =>F[A16], a17: =>F[A17], a18: =>F[A18])(
        f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => R)(
        implicit A: Apply[F]): F[R] = {
      import A._
      ap(a18)(ap(a17)(ap(a16)(ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(
        ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(map(a1)(f.curried))))))))))))))))))
    }
    def apply19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
                A15, A16, A17, A18, A19, R](
        a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
        a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10],
        a11: =>F[A11], a12: =>F[A12], a13: =>F[A13], a14: =>F[A14], a15: =>F[A15],
        a16: =>F[A16], a17: =>F[A17], a18: =>F[A18], a19: =>F[A19])(
        f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
            A15, A16, A17, A18, A19) => R)(
        implicit A: Apply[F]): F[R] = {
      import A._
      ap(a19)(ap(a18)(ap(a17)(ap(a16)(ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(
        ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(map(a1)(f.curried)))))))))))))))))))
    }
    def apply20[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
                A15, A16, A17, A18, A19, A20, R](
        a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
        a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10],
        a11: =>F[A11], a12: =>F[A12], a13: =>F[A13], a14: =>F[A14], a15: =>F[A15],
        a16: =>F[A16], a17: =>F[A17], a18: =>F[A18], a19: =>F[A19], a20: =>F[A20])(
        f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
            A15, A16, A17, A18, A19, A20) => R)(
        implicit A: Apply[F]): F[R] = {
      import A._
      ap(a20)(ap(a19)(ap(a18)(ap(a17)(ap(a16)(ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(
        ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(map(a1)(f.curried))))))))))))))))))))
    }
    def apply21[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
                A15, A16, A17, A18, A19, A20, A21, R](
        a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
        a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10],
        a11: =>F[A11], a12: =>F[A12], a13: =>F[A13], a14: =>F[A14], a15: =>F[A15],
        a16: =>F[A16], a17: =>F[A17], a18: =>F[A18], a19: =>F[A19], a20: =>F[A20],
        a21: =>F[A21])(
        f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
            A15, A16, A17, A18, A19, A20, A21) => R)(
        implicit A: Apply[F]): F[R] = {
      import A._
      ap(a21)(ap(a20)(ap(a19)(ap(a18)(ap(a17)(ap(a16)(ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(
        ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(map(a1)(f.curried)))))))))))))))))))))
    }
    def apply22[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
                A15, A16, A17, A18, A19, A20, A21, A22, R](
        a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
        a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10],
        a11: =>F[A11], a12: =>F[A12], a13: =>F[A13], a14: =>F[A14], a15: =>F[A15],
        a16: =>F[A16], a17: =>F[A17], a18: =>F[A18], a19: =>F[A19], a20: =>F[A20],
        a21: =>F[A21], a22: =>F[A22])(
        f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
            A15, A16, A17, A18, A19, A20, A21, A22) => R)(
        implicit A: Apply[F]): F[R] = {
      import A._
      ap(a22)(ap(a21)(ap(a20)(ap(a19)(ap(a18)(ap(a17)(ap(a16)(ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(
        ap(a10)(ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(
          map(a1)(f.curried))))))))))))))))))))))
    }
}
