package bz

import scala.language.higherKinds
import scalaz.{Applicative, \/}
import scalaz.syntax.either._

// this is in scalaz bleeding edge, we need it here til it gets released ga
trait Alt[F[_]] extends Applicative[F] { self =>
  ////

  def alt[A](a1: =>F[A], a2: =>F[A]): F[A]

  /** One or none */
  def altly1[Z, A1](a1: =>F[A1])(f: A1 => Z): F[Z] = map(a1)(f)
  def altly2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: A1 \/ A2 => Z): F[Z] =
    map(alt(map(a1)(_.left[A2]), map(a2)(_.right[A1])))(f)

  def altly3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: A1 \/ (A2 \/ A3) => Z
  ): F[Z] = altly2(a1, altly2(a2, a3)(identity))(f)
  def altly4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z
  ): F[Z] = altly2(a1, altly3(a2, a3, a4)(identity))(f)
  def altly5[Z, A1, A2, A3, A4, A5]
    (a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
     a5: =>F[A5])(
    f: A1 \/ (A2 \/ (A3 \/ (A4 \/ A5))) => Z
  ): F[Z] = altly2(a1, altly4(a2, a3, a4, a5)(identity))(f)
  def altly6[Z, A1, A2, A3, A4, A5, A6]
    (a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
     a5: =>F[A5], a6: =>F[A6])(
    f: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6)))) => Z
  ): F[Z] = altly2(a1, altly5(a2, a3, a4, a5, a6)(identity))(f)
  def altly7[Z, A1, A2, A3, A4, A5, A6, A7]
    (a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
     a5: =>F[A5], a6: =>F[A6], a7: =>F[A7])(
    f: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7))))) => Z
  ): F[Z] = altly2(a1, altly6(a2, a3, a4, a5, a6, a7)(identity))(f)
  def altly8[Z, A1, A2, A3, A4, A5, A6, A7, A8]
    (a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
     a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8])(
    f: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8)))))) => Z
  ): F[Z] = altly2(a1, altly7(a2, a3, a4, a5, a6, a7, a8)(identity))(f)
  def altly9[Z, A1, A2, A3, A4, A5, A6, A7, A8, A9]
    (a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
     a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
     a9: =>F[A9])(
    f: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9))))))) => Z
  ): F[Z] = altly2(a1, altly8(a2, a3, a4, a5, a6, a7, a8, a9)(identity))(f)
  def altly10[Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
    (a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
     a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
     a9: =>F[A9], a10: =>F[A10])(
    f: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))) => Z
  ): F[Z] = altly2(a1, altly9(a2, a3, a4, a5, a6, a7, a8, a9, a10)(identity))(f)
  def altly11[Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
    (a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
     a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
     a9: =>F[A9], a10: =>F[A10], a11: =>F[A11])(
    f: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))) => Z
  ): F[Z] = altly2(a1, altly10(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(identity))(f)
  def altly12[Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
    (a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
     a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
     a9: =>F[A9], a10: =>F[A10], a11: =>F[A11], a12: =>F[A12])(
    f: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))) => Z
  ): F[Z] = altly2(a1, altly11(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(identity))(f)
  def altly13[Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]
    (a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
     a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
     a9: =>F[A9], a10: =>F[A10], a11: =>F[A11], a12: =>F[A12],
     a13: =>F[A13])(
    f: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
       (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))) => Z
  ): F[Z] = altly2(a1, altly12(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(identity))(f)
  def altly14[Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
    (a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4],
     a5: =>F[A5], a6: =>F[A6], a7: =>F[A7], a8: =>F[A8],
     a9: =>F[A9], a10: =>F[A10], a11: =>F[A11], a12: =>F[A12],
     a13: =>F[A13], a14: =>F[A14])(
    f: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
       (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))) => Z
  ): F[Z] = altly2(a1, altly13(a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(identity))(f)
}
