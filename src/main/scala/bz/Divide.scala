package bz

import scala.language.higherKinds
import scalaz.Contravariant

// this is in scalaz bleeding edge, we need it here til it gets released ga
trait Divide[F[_]] extends Contravariant[F] { self =>
  ////
  final def divide[A, B, C](fa: =>F[A], fb: =>F[B])(f: C => (A, B)): F[C] =
    divide2(fa, fb)(f)

  final def divide1[A1, Z](a1: F[A1])(f: Z => A1): F[Z] = contramap(a1)(f)

  def tuple2[A1, A2](a1: =>F[A1], a2: =>F[A2]): F[(A1, A2)] = divide2(a1, a2)(identity)

  def divide2[A1, A2, Z](a1: =>F[A1], a2: =>F[A2])(f: Z => (A1, A2)): F[Z]
  def divide3[A1, A2, A3, Z](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: Z => (A1, A2, A3)
  ): F[Z] = divide2(tuple2(a1, a2), a3) { z =>
    val t = f(z)
    ((t._1, t._2), t._3)
  }
  def divide4[A1, A2, A3, A4, Z](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: Z => (A1, A2, A3, A4)
  ): F[Z] = divide2(tuple2(a1, a2), tuple2(a3, a4)) { z =>
    val t = f(z)
    ((t._1, t._2), (t._3, t._4))
  }
  def divide5[A1, A2, A3, A4, A5, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5])(
    f: Z => (A1, A2, A3, A4, A5)
  ): F[Z] = divide2(tuple2(tuple2(a1, a2), a3), tuple2(a4, a5)) { z =>
    val t = f(z)
    (((t._1, t._2), t._3), (t._4, t._5))
  }
  def divide6[A1, A2, A3, A4, A5, A6, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6])(
    f: Z => (A1, A2, A3, A4, A5, A6)
  ): F[Z] = divide2(tuple2(tuple2(a1, a2), tuple2(a3, a4)), tuple2(a5, a6)) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (t._5, t._6))
  }
  def divide7[A1, A2, A3, A4, A5, A6, A7, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7)
  ): F[Z] = divide2(tuple2(tuple2(a1, a2), tuple2(a3, a4)), tuple2(tuple2(a5, a6), a7)) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), ((t._5, t._6), t._7))
  }
  def divide8[A1, A2, A3, A4, A5, A6, A7, A8, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8)
  ): F[Z] = divide2(tuple2(tuple2(a1, a2), tuple2(a3, a4)), tuple2(tuple2(a5, a6), tuple2(a7, a8))) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), ((t._5, t._6), (t._7, t._8)))
  }
  def divide9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
    a9: =>F[A9])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9)
  ): F[Z] = divide2(tuple2(tuple2(a1, a2), tuple2(a3, a4)),
                    tuple2(tuple2(tuple2(a5, a6), tuple2(a7, a8)), a9)) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (((t._5, t._6), (t._7, t._8)), t._9))
  }
  def divide10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
    a9: =>F[A9], a10: =>F[A10])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)
  ): F[Z] = divide2(tuple2(tuple2(a1, a2), tuple2(a3, a4)),
                    tuple2(tuple2(tuple2(a5, a6), tuple2(a7, a8)), tuple2(a9, a10))) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (((t._5, t._6), (t._7, t._8)), (t._9, t._10)))
  }
  def divide11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
    a9: =>F[A9], a10: =>F[A10], a11: =>F[A11])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)
  ): F[Z] = divide2(tuple2(
                      tuple2(a1, a2),
                      tuple2(a3, a4)),
                    tuple2(
                      tuple2(tuple2(a5, a6), tuple2(a7, a8)),
                      tuple2(a9, tuple2(a10, a11)))) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (((t._5, t._6), (t._7, t._8)), (t._9, (t._10, t._11))))
  }
  def divide12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
    a9: =>F[A9], a10: =>F[A10], a11: =>F[A11], a12: =>F[A12])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
  ): F[Z] = divide2(tuple2(
                      tuple2(a1, a2),
                      tuple2(a3, a4)),
                    tuple2(
                      tuple2(tuple2(a5, a6), tuple2(a7, a8)),
                      tuple2(tuple2(a9, a10), tuple2(a11, a12)))) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (((t._5, t._6), (t._7, t._8)), ((t._9, t._10), (t._11, t._12))))
  }
  def divide13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
    a9: =>F[A9], a10: =>F[A10], a11: =>F[A11], a12: =>F[A12],
    a13: =>F[A13])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)
  ): F[Z] = divide2(tuple2(
                      tuple2(a1, a2),
                      tuple2(a3, a4)),
                    tuple2(
                      tuple2(tuple2(a5, a6), tuple2(a7, a8)),
                      tuple2(tuple2(a9, a10), tuple2(a11, tuple2(a12, a13))))) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (((t._5, t._6), (t._7, t._8)), ((t._9, t._10),
      (t._11, (t._12, t._13)))))
  }
  def divide14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
    a9: =>F[A9], a10: =>F[A10], a11: =>F[A11], a12: =>F[A12],
    a13: =>F[A13], a14: =>F[A14])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)
  ): F[Z] = divide2(tuple2(
                      tuple2(a1, a2),
                      tuple2(a3, a4)),
                    tuple2(
                      tuple2(tuple2(a5, a6), tuple2(a7, a8)),
                      tuple2(tuple2(a9, a10),
                        tuple2(tuple2(a11, a12), tuple2(a13, a14))))) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (((t._5, t._6), (t._7, t._8)), ((t._9, t._10),
      ((t._11, t._12), (t._13, t._14)))))
  }
  def divide15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
    a9: =>F[A9], a10: =>F[A10], a11: =>F[A11], a12: =>F[A12],
    a13: =>F[A13], a14: =>F[A14], a15: =>F[A15])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)
  ): F[Z] = divide2(tuple2(
                      tuple2(a1, a2),
                      tuple2(a3, a4)),
                    tuple2(
                      tuple2(tuple2(a5, a6), tuple2(a7, a8)),
                      tuple2(tuple2(a9, a10),
                        tuple2(tuple2(a11, a12),
                        tuple2(a13, tuple2(a14, a15)))))) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (((t._5, t._6), (t._7, t._8)), ((t._9, t._10),
      ((t._11, t._12), (t._13, (t._14, t._15))))))
  }
  def divide16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
    a9: =>F[A9], a10: =>F[A10], a11: =>F[A11], a12: =>F[A12],
    a13: =>F[A13], a14: =>F[A14], a15: =>F[A15], a16: =>F[A16])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
  ): F[Z] = divide2(tuple2(
                      tuple2(a1, a2),
                      tuple2(a3, a4)),
                    tuple2(
                      tuple2(tuple2(a5, a6), tuple2(a7, a8)),
                      tuple2(tuple2(a9, a10),
                        tuple2(tuple2(a11, a12),
                          tuple2(
                            tuple2(a13, a14),
                            tuple2(a15, a16)))))) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (((t._5, t._6), (t._7, t._8)), ((t._9, t._10),
      ((t._11, t._12), ((t._13, t._14), (t._15, t._16))))))
  }
  def divide17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
    a9: =>F[A9], a10: =>F[A10], a11: =>F[A11], a12: =>F[A12],
    a13: =>F[A13], a14: =>F[A14], a15: =>F[A15], a16: =>F[A16],
    a17: =>F[A17])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)
  ): F[Z] = divide2(tuple2(
                      tuple2(a1, a2),
                      tuple2(a3, a4)),
                    tuple2(
                      tuple2(tuple2(a5, a6), tuple2(a7, a8)),
                      tuple2(tuple2(a9, a10),
                        tuple2(tuple2(a11, a12),
                          tuple2(
                            tuple2(a13, a14),
                            tuple2(a15, tuple2(a16, a17))))))) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (((t._5, t._6), (t._7, t._8)), ((t._9, t._10),
      ((t._11, t._12), ((t._13, t._14), (t._15, (t._16, t._17)))))))
  }
  def divide18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, A18, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
    a9: =>F[A9], a10: =>F[A10], a11: =>F[A11], a12: =>F[A12],
    a13: =>F[A13], a14: =>F[A14], a15: =>F[A15], a16: =>F[A16],
    a17: =>F[A17], a18: =>F[A18])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)
  ): F[Z] = divide2(tuple2(
                      tuple2(a1, a2),
                      tuple2(a3, a4)),
                    tuple2(
                      tuple2(tuple2(a5, a6), tuple2(a7, a8)),
                      tuple2(tuple2(a9, a10),
                        tuple2(tuple2(a11, a12),
                          tuple2(
                            tuple2(a13, a14),
                            tuple2(tuple2(a15, a16),
                              tuple2(a17, a18))))))) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (((t._5, t._6), (t._7, t._8)), ((t._9, t._10),
      ((t._11, t._12), ((t._13, t._14), ((t._15, t._16), (t._17, t._18)))))))
  }
  def divide19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, A18, A19, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
    a9: =>F[A9], a10: =>F[A10], a11: =>F[A11], a12: =>F[A12],
    a13: =>F[A13], a14: =>F[A14], a15: =>F[A15], a16: =>F[A16],
    a17: =>F[A17], a18: =>F[A18], a19: =>F[A19])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
             A15, A16, A17, A18, A19)
  ): F[Z] = divide2(tuple2(
                      tuple2(a1, a2),
                      tuple2(a3, a4)),
                    tuple2(
                      tuple2(tuple2(a5, a6), tuple2(a7, a8)),
                      tuple2(tuple2(a9, a10),
                        tuple2(tuple2(a11, a12),
                          tuple2(
                            tuple2(a13, a14),
                            tuple2(tuple2(a15, a16),
                              tuple2(a17, tuple2(a18, a19)))))))) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (((t._5, t._6), (t._7, t._8)), ((t._9, t._10),
      ((t._11, t._12), ((t._13, t._14), ((t._15, t._16), (t._17, (t._18, t._19))))))))
  }
  def divide20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, A18, A19, A20, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
    a9: =>F[A9], a10: =>F[A10], a11: =>F[A11], a12: =>F[A12],
    a13: =>F[A13], a14: =>F[A14], a15: =>F[A15], a16: =>F[A16],
    a17: =>F[A17], a18: =>F[A18], a19: =>F[A19], a20: =>F[A20])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
             A15, A16, A17, A18, A19, A20)
  ): F[Z] = divide2(tuple2(
                      tuple2(a1, a2),
                      tuple2(a3, a4)),
                    tuple2(
                      tuple2(tuple2(a5, a6), tuple2(a7, a8)),
                      tuple2(tuple2(a9, a10),
                        tuple2(tuple2(a11, a12),
                          tuple2(
                            tuple2(a13, a14),
                            tuple2(tuple2(a15, a16),
                              tuple2(tuple2(a17, a18), tuple2(a19, a20)))))))) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (((t._5, t._6), (t._7, t._8)), ((t._9, t._10),
      ((t._11, t._12), ((t._13, t._14), ((t._15, t._16), ((t._17, t._18), (t._19, t._20))))))))
  }
  def divide21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, A18, A19, A20, A21, Z](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
    a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
    a9: =>F[A9], a10: =>F[A10], a11: =>F[A11], a12: =>F[A12],
    a13: =>F[A13], a14: =>F[A14], a15: =>F[A15], a16: =>F[A16],
    a17: =>F[A17], a18: =>F[A18], a19: =>F[A19], a20: =>F[A20],
    a21: =>F[A21])(
    f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
             A15, A16, A17, A18, A19, A20, A21)
  ): F[Z] = divide2(tuple2(
                      tuple2(a1, a2),
                      tuple2(a3, a4)),
                    tuple2(
                      tuple2(tuple2(a5, a6), tuple2(a7, a8)),
                      tuple2(tuple2(a9, a10),
                        tuple2(tuple2(a11, a12),
                          tuple2(
                            tuple2(a13, a14),
                            tuple2(tuple2(a15, a16),
                              tuple2(tuple2(a17, a18),
                                tuple2(a19, tuple2(a20, a21))))))))) { z =>
    val t = f(z)
    (((t._1, t._2), (t._3, t._4)), (((t._5, t._6), (t._7, t._8)), ((t._9, t._10),
      ((t._11, t._12), ((t._13, t._14), ((t._15, t._16), ((t._17, t._18),
        (t._19, (t._20, t._21)))))))))
  }
}
