package bz

import scala.language.higherKinds
import scalaz.\/

// this is in scalaz bleeding edge, need it here til it gets released ga
trait Decidable[F[_]] {
  def choose2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: Z => (A1 \/ A2)): F[Z]

  def choose3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: Z => A1 \/ (A2 \/ A3)
  ): F[Z] = {
    val a23: F[A2 \/ A3] = choose2(a2, a3)(identity)
    choose2(a1, a23)(f)
  }
  def choose4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: Z => A1 \/ (A2 \/ (A3 \/ A4))
  ): F[Z] = {
    val a234: F[A2 \/ (A3 \/ A4)] = choose3(a2, a3, a4)(identity)
    choose2(a1, a234)(f)
  }
  def choose5[Z, A1, A2, A3, A4, A5](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5])(
    f: Z => A1 \/ (A2 \/ (A3 \/ (A4 \/ A5)))
  ): F[Z] = {
    val a2345: F[A2 \/ (A3 \/ (A4 \/ A5))] = choose4(a2, a3, a4, a5)(identity)
    choose2(a1, a2345)(f)
  }
  def choose6[Z, A1, A2, A3, A4, A5, A6](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5], a6: =>F[A6])(
    f: Z => A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6))))
  ): F[Z] = {
    val a23456: F[A2 \/ (A3 \/ (A4 \/ (A5 \/ A6)))] = choose5(a2, a3, a4, a5, a6)(identity)
    choose2(a1, a23456)(f)
  }
  def choose7[Z, A1, A2, A3, A4, A5, A6, A7](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5], a6: =>F[A6], a7: =>F[A7])(
    f: Z => A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))))
  ): F[Z] = {
    val a234567: F[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7))))] = choose6(a2, a3, a4, a5, a6, a7)(identity)
    choose2(a1, a234567)(f)
  }
  def choose8[Z, A1, A2, A3, A4, A5, A6, A7, A8](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
    a6: =>F[A6], a7: =>F[A7], a8: =>F[A8])(
    f: Z => A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))
  ): F[Z] = {
    val a2345678: F[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8)))))] = choose7(a2, a3, a4, a5, a6, a7, a8)(identity)
    choose2(a1, a2345678)(f)
  }
  def choose9[Z, A1, A2, A3, A4, A5, A6, A7, A8, A9](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
    a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9])(
    f: Z => A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))
  ): F[Z] = {
    val a23456789: F[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9))))))] = choose8(a2, a3, a4, a5, a6, a7, a8, a9)(identity)
    choose2(a1, a23456789)(f)
  }
  def choose10[Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
    a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10])(
    f: Z => A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))
  ): F[Z] = {
    val a2345678910: F[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))] = choose9(a2, a3, a4, a5, a6, a7, a8, a9, a10)(identity)
    choose2(a1, a2345678910)(f)
  }
  def choose11[Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
    a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10], a11: =>F[A11])(
    f: Z => A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))
  ): F[Z] = {
    val a: F[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))] = choose10(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(identity)
    choose2(a1, a)(f)
  }
  def choose12[Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
    a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10],
    a11: =>F[A11], a12: =>F[A12])(
    f: Z => A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))
  ): F[Z] = {
    val a: F[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))] =
      choose11(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(identity)
    choose2(a1, a)(f)
  }
  def choose13[Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
    a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10],
    a11: =>F[A11], a12: =>F[A12], a13: =>F[A13])(
    f: Z => A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))
  ): F[Z] = {
    val a: F[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))] =
      choose12(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(identity)
    choose2(a1, a)(f)
  }
  def choose14[Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5],
    a6: =>F[A6], a7: =>F[A7], a8: =>F[A8], a9: =>F[A9], a10: =>F[A10],
    a11: =>F[A11], a12: =>F[A12], a13: =>F[A13], a14: =>F[A14])(
    f: Z => A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
						(A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))
  ): F[Z] = {
    val a: F[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
						(A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))] =
      choose13(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(identity)
    choose2(a1, a)(f)
  }
}
