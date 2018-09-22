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
}