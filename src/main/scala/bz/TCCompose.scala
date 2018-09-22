package bz

import iotaz.{Cop => ICop, Prod => IProd, TList, TNil}
import iotaz.TList.::
import scala.language.higherKinds
import scalaz.{Apply, Isomorphism => iso, \/}
import iso.<=>

trait TC {
  type TL <: TList
  type Cop
  type Prod
  def copIso: (ICop[TL] <=> Cop)
  def prodIso: (IProd[TL] <=> Prod)
  // def choose[F[_]](implicit d: Decidable[F]): F[ICop[TL]]
  // def alt[F[_]](implicit a: Alt[F]): F[ICop[TL]]
  // def divide[F[_]](implicit a: Divide[F]): F[IProd[TL]]
  // def apply[F[_]](implicit a: Apply[F]): F[IProd[TL]]
}

trait TC1[A1] extends TC {
  type TL = A1 :: TNil
  type Cop = A1
  type Prod = A1
  val copIso = iso.IsoSet(
    Cops.to1(_: ICop[TL]),
    Cops.from1(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to1T(_: IProd[TL]),
    Prods.from1T(_: Prod))
}

trait TC2[A1, A2] extends TC {
  type TL = A1 :: A2 :: TNil
  type Cop = (A1 \/ A2)
  type Prod = (A1, A2)
  val copIso = iso.IsoSet(
    Cops.to2(_: ICop[TL]),
    Cops.from2(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to2T(_: IProd[TL]),
    Prods.from2T(_: Prod))
  def choose[F[_]](implicit d: Decidable[F], a1: F[A1], a2: F[A2]): F[ICop[TL]] =
    d.choose2(a1, a2)(copIso.to(_))
  def alt[F[_]](implicit a: Alt[F], a1: F[A1], a2: F[A2]): F[ICop[TL]] =
    a.altly2(a1, a2)(copIso.from(_))
  def divide[F[_]](implicit d: Divide[F], a1: F[A1], a2: F[A2]): F[IProd[TL]] =
    d.divide2(a1, a2)(prodIso.to(_))
  def apply[F[_]](implicit d: Apply[F], a1: F[A1], a2: F[A2]): F[IProd[TL]] =
    d.apply2(a1, a2)((a, b) => prodIso.from((a, b)))
}

trait TC3[A1, A2, A3] extends TC {
  type TL = A1 :: A2 :: A3 :: TNil
  type Cop = (A1 \/ (A2 \/ A3))
  type Prod = (A1, A2, A3)
  val copIso = iso.IsoSet(
    Cops.to3(_: ICop[TL]),
    Cops.from3(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to3T(_: IProd[TL]),
    Prods.from3T(_: Prod))
  def choose[F[_]](
      implicit d: Decidable[F], a1: F[A1], a2: F[A2], a3: F[A3]): F[ICop[TL]] =
    d.choose3(a1, a2, a3)(copIso.to(_))
  def alt[F[_]](implicit a: Alt[F], a1: F[A1], a2: F[A2], a3: F[A3]): F[ICop[TL]] =
    a.altly3(a1, a2, a3)(copIso.from(_))
  def divide[F[_]](implicit d: Divide[F], a1: F[A1], a2: F[A2], a3: F[A3]): F[IProd[TL]] =
    d.divide3(a1, a2, a3)(prodIso.to(_))
  def apply[F[_]](implicit d: Apply[F], a1: F[A1], a2: F[A2], a3: F[A3]): F[IProd[TL]] =
    d.apply3(a1, a2, a3)((a, b, c) => prodIso.from((a, b, c)))
}

trait TC4[A1, A2, A3, A4] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ A4)))
  type Prod = (A1, A2, A3, A4)
  val copIso = iso.IsoSet(
    Cops.to4(_: ICop[TL]),
    Cops.from4(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to4T(_: IProd[TL]),
    Prods.from4T(_: Prod))
  def choose[F[_]](
      implicit d: Decidable[F], a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[ICop[TL]] =
    d.choose4(a1, a2, a3, a4)(copIso.to(_))
  def alt[F[_]](implicit a: Alt[F], a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[ICop[TL]] =
    a.altly4(a1, a2, a3, a4)(copIso.from(_))
  def divide[F[_]](implicit d: Divide[F], a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[IProd[TL]] =
    d.divide4(a1, a2, a3, a4)(prodIso.to(_))
  def apply[F[_]](implicit d: Apply[F], a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[IProd[TL]] =
    d.apply4(a1, a2, a3, a4)((a, b, c, d) => prodIso.from((a, b, c, d)))
}

trait TC5[A1, A2, A3, A4, A5] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ A5))))
  type Prod = (A1, A2, A3, A4, A5)
  val copIso = iso.IsoSet(
    Cops.to5(_: ICop[TL]),
    Cops.from5(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to5T(_: IProd[TL]),
    Prods.from5T(_: Prod))
  def choose[F[_]](implicit d: Decidable[F], a1: F[A1], a2: F[A2],
                   a3: F[A3], a4: F[A4], a5: F[A5]): F[ICop[TL]] =
    d.choose5(a1, a2, a3, a4, a5)(copIso.to(_))
  def alt[F[_]](implicit a: Alt[F], a1: F[A1], a2: F[A2], a3: F[A3],
                a4: F[A4], a5: F[A5]): F[ICop[TL]] =
    a.altly5(a1, a2, a3, a4, a5)(copIso.from(_))
  def divide[F[_]](implicit d: Divide[F], a1: F[A1], a2: F[A2], a3: F[A3],
                   a4: F[A4], a5: F[A5]): F[IProd[TL]] =
    d.divide5(a1, a2, a3, a4, a5)(prodIso.to(_))
  def apply[F[_]](implicit d: Apply[F], a1: F[A1], a2: F[A2], a3: F[A3],
                  a4: F[A4], a5: F[A5]): F[IProd[TL]] =
    d.apply5(a1, a2, a3, a4, a5)((a, b, c, d, e) => prodIso.from((a, b, c, d, e)))
}

trait TC6[A1, A2, A3, A4, A5, A6] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6)))))
  type Prod = (A1, A2, A3, A4, A5, A6)
  val copIso = iso.IsoSet(
    Cops.to6(_: ICop[TL]),
    Cops.from6(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to6T(_: IProd[TL]),
    Prods.from6T(_: Prod))
  def choose[F[_]](implicit d: Decidable[F], a1: F[A1], a2: F[A2],
                   a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6]): F[ICop[TL]] =
    d.choose6(a1, a2, a3, a4, a5, a6)(copIso.to(_))
  def alt[F[_]](implicit a: Alt[F], a1: F[A1], a2: F[A2], a3: F[A3],
                a4: F[A4], a5: F[A5], a6: F[A6]): F[ICop[TL]] =
    a.altly6(a1, a2, a3, a4, a5, a6)(copIso.from(_))
  def divide[F[_]](implicit d: Divide[F], a1: F[A1], a2: F[A2], a3: F[A3],
                   a4: F[A4], a5: F[A5], a6: F[A6]): F[IProd[TL]] =
    d.divide6(a1, a2, a3, a4, a5, a6)(prodIso.to(_))
  def apply[F[_]](implicit d: Apply[F], a1: F[A1], a2: F[A2], a3: F[A3],
                  a4: F[A4], a5: F[A5], a6: F[A6]): F[IProd[TL]] =
    d.apply6(a1, a2, a3, a4, a5, a6)((a, b, c, d, e, f) => prodIso.from((a, b, c, d, e, f)))
}

trait TC7[A1, A2, A3, A4, A5, A6, A7] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7)
  val copIso = iso.IsoSet(
    Cops.to7(_: ICop[TL]),
    Cops.from7(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to7T(_: IProd[TL]),
    Prods.from7T(_: Prod))
  def choose[F[_]](implicit d: Decidable[F], a1: F[A1], a2: F[A2],
                   a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6],
                   a7: F[A7]): F[ICop[TL]] =
    d.choose7(a1, a2, a3, a4, a5, a6, a7)(copIso.to(_))
  def alt[F[_]](implicit a: Alt[F], a1: F[A1], a2: F[A2], a3: F[A3],
                a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7]): F[ICop[TL]] =
    a.altly7(a1, a2, a3, a4, a5, a6, a7)(copIso.from(_))
  def divide[F[_]](implicit d: Divide[F], a1: F[A1], a2: F[A2], a3: F[A3],
                   a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7]): F[IProd[TL]] =
    d.divide7(a1, a2, a3, a4, a5, a6, a7)(prodIso.to(_))
  def apply[F[_]](implicit d: Apply[F], a1: F[A1], a2: F[A2], a3: F[A3],
                  a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7]): F[IProd[TL]] =
    d.apply7(a1, a2, a3, a4, a5, a6, a7)((a, b, c, d, e, f, g) => prodIso.from((a, b, c, d, e, f, g)))
}

trait TC8[A1, A2, A3, A4, A5, A6, A7, A8] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8)))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8)
  val copIso = iso.IsoSet(
    Cops.to8(_: ICop[TL]),
    Cops.from8(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to8T(_: IProd[TL]),
    Prods.from8T(_: Prod))
  def choose[F[_]](implicit d: Decidable[F], a1: F[A1], a2: F[A2],
                   a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6],
                   a7: F[A7], a8: F[A8]): F[ICop[TL]] =
    d.choose8(a1, a2, a3, a4, a5, a6, a7, a8)(copIso.to(_))
  def alt[F[_]](implicit a: Alt[F], a1: F[A1], a2: F[A2], a3: F[A3],
                a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8]): F[ICop[TL]] =
    a.altly8(a1, a2, a3, a4, a5, a6, a7, a8)(copIso.from(_))
  def divide[F[_]](implicit d: Divide[F], a1: F[A1], a2: F[A2], a3: F[A3],
                   a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                   a8: F[A8]): F[IProd[TL]] =
    d.divide8(a1, a2, a3, a4, a5, a6, a7, a8)(prodIso.to(_))
  def apply[F[_]](implicit d: Apply[F], a1: F[A1], a2: F[A2], a3: F[A3],
                  a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                  a8: F[A8]): F[IProd[TL]] =
    d.apply8(a1, a2, a3, a4, a5, a6, a7, a8)((a, b, c, d, e, f, g, h) =>
      prodIso.from((a, b, c, d, e, f, g, h)))
}

trait TC9[A1, A2, A3, A4, A5, A6, A7, A8, A9] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9)
  val copIso = iso.IsoSet(
    Cops.to9(_: ICop[TL]),
    Cops.from9(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to9T(_: IProd[TL]),
    Prods.from9T(_: Prod))
  def choose[F[_]](implicit d: Decidable[F], a1: F[A1], a2: F[A2],
                   a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6],
                   a7: F[A7], a8: F[A8], a9: F[A9]): F[ICop[TL]] =
    d.choose9(a1, a2, a3, a4, a5, a6, a7, a8, a9)(copIso.to(_))
  def alt[F[_]](implicit a: Alt[F], a1: F[A1], a2: F[A2], a3: F[A3],
                a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                a9: F[A9]): F[ICop[TL]] =
    a.altly9(a1, a2, a3, a4, a5, a6, a7, a8, a9)(copIso.from(_))
  def divide[F[_]](implicit d: Divide[F], a1: F[A1], a2: F[A2], a3: F[A3],
                   a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                   a8: F[A8], a9: F[A9]): F[IProd[TL]] =
    d.divide9(a1, a2, a3, a4, a5, a6, a7, a8, a9)(prodIso.to(_))
  def apply[F[_]](implicit d: Apply[F], a1: F[A1], a2: F[A2], a3: F[A3],
                  a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                  a8: F[A8], a9: F[A9]): F[IProd[TL]] =
    d.apply9(a1, a2, a3, a4, a5, a6, a7, a8, a9)((a, b, c, d, e, f, g, h, i) =>
      prodIso.from((a, b, c, d, e, f, g, h, i)))
}

trait TC10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)
  val copIso = iso.IsoSet(
    Cops.to10(_: ICop[TL]),
    Cops.from10(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to10T(_: IProd[TL]),
    Prods.from10T(_: Prod))
  def choose[F[_]](implicit d: Decidable[F], a1: F[A1], a2: F[A2],
                   a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6],
                   a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10]): F[ICop[TL]] =
    d.choose10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(copIso.to(_))
  def alt[F[_]](implicit a: Alt[F], a1: F[A1], a2: F[A2], a3: F[A3],
                a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                a9: F[A9], a10: F[A10]): F[ICop[TL]] =
    a.altly10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(copIso.from(_))
  def divide[F[_]](implicit d: Divide[F], a1: F[A1], a2: F[A2], a3: F[A3],
                   a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                   a8: F[A8], a9: F[A9], a10: F[A10]): F[IProd[TL]] =
    d.divide10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(prodIso.to(_))
  def apply[F[_]](implicit d: Apply[F], a1: F[A1], a2: F[A2], a3: F[A3],
                  a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                  a8: F[A8], a9: F[A9], a10: F[A10]): F[IProd[TL]] =
    d.apply10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)((a, b, c, d, e, f, g, h, i, j) =>
      prodIso.from((a, b, c, d, e, f, g, h, i, j)))
}

trait TC11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: A11 ::TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)
  val copIso = iso.IsoSet(
    Cops.to11(_: ICop[TL]),
    Cops.from11(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to11T(_: IProd[TL]),
    Prods.from11T(_: Prod))
  def choose[F[_]](implicit d: Decidable[F], a1: F[A1], a2: F[A2],
                   a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6],
                   a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10],
                   a11: F[A11]): F[ICop[TL]] =
    d.choose11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(copIso.to(_))
  def alt[F[_]](implicit a: Alt[F], a1: F[A1], a2: F[A2], a3: F[A3],
                a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                a9: F[A9], a10: F[A10], a11: F[A11]): F[ICop[TL]] =
    a.altly11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(copIso.from(_))
  def divide[F[_]](implicit d: Divide[F], a1: F[A1], a2: F[A2], a3: F[A3],
                   a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                   a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11]): F[IProd[TL]] =
    d.divide11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(prodIso.to(_))
  def apply[F[_]](implicit d: Apply[F], a1: F[A1], a2: F[A2], a3: F[A3],
                  a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                  a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11]): F[IProd[TL]] =
    d.apply11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(
      (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11) =>
        prodIso.from((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11)))
}

trait TC12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: A11 :: A12 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
  val copIso = iso.IsoSet(
    Cops.to12(_: ICop[TL]),
    Cops.from12(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to12T(_: IProd[TL]),
    Prods.from12T(_: Prod))
  def choose[F[_]](implicit d: Decidable[F], a1: F[A1], a2: F[A2],
                   a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6],
                   a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10],
                   a11: F[A11], a12: F[A12]): F[ICop[TL]] =
    d.choose12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(copIso.to(_))
  def alt[F[_]](implicit a: Alt[F], a1: F[A1], a2: F[A2], a3: F[A3],
                a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12]): F[ICop[TL]] =
    a.altly12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(copIso.from(_))
  def divide[F[_]](implicit d: Divide[F], a1: F[A1], a2: F[A2], a3: F[A3],
                   a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                   a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11],
                   a12: F[A12]): F[IProd[TL]] =
    d.divide12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(prodIso.to(_))
  def apply[F[_]](implicit d: Apply[F], a1: F[A1], a2: F[A2], a3: F[A3],
                  a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                  a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11],
                  a12: F[A12]): F[IProd[TL]] =
    d.apply12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(
      (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12) =>
        prodIso.from((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12)))
}

trait TC13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)
  val copIso = iso.IsoSet(
    Cops.to13(_: ICop[TL]),
    Cops.from13(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to13T(_: IProd[TL]),
    Prods.from13T(_: Prod))
  def choose[F[_]](implicit d: Decidable[F], a1: F[A1], a2: F[A2],
                   a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6],
                   a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10],
                   a11: F[A11], a12: F[A12], a13: F[A13]): F[ICop[TL]] =
    d.choose13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(copIso.to(_))
  def alt[F[_]](implicit a: Alt[F], a1: F[A1], a2: F[A2], a3: F[A3],
                a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13]): F[ICop[TL]] =
    a.altly13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(copIso.from(_))
  def divide[F[_]](implicit d: Divide[F], a1: F[A1], a2: F[A2], a3: F[A3],
                   a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                   a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11],
                   a12: F[A12], a13: F[A13]): F[IProd[TL]] =
    d.divide13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(prodIso.to(_))
  def apply[F[_]](implicit d: Apply[F], a1: F[A1], a2: F[A2], a3: F[A3],
                  a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                  a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11],
                  a12: F[A12], a13: F[A13]): F[IProd[TL]] =
    ApplyExt.apply13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(
      (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13) =>
        prodIso.from((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13)))
}

trait TC14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 ::TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)
  val copIso = iso.IsoSet(
    Cops.to14(_: ICop[TL]),
    Cops.from14(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to14T(_: IProd[TL]),
    Prods.from14T(_: Prod))
  def choose[F[_]](implicit d: Decidable[F], a1: F[A1], a2: F[A2],
                   a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6],
                   a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10],
                   a11: F[A11], a12: F[A12], a13: F[A13],
                   a14: F[A14]): F[ICop[TL]] =
    d.choose14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(copIso.to(_))
  def alt[F[_]](implicit a: Alt[F], a1: F[A1], a2: F[A2], a3: F[A3],
                a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13],
                a14: F[A14]): F[ICop[TL]] =
    a.altly14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(copIso.from(_))
  def divide[F[_]](implicit d: Divide[F], a1: F[A1], a2: F[A2], a3: F[A3],
                   a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                   a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11],
                   a12: F[A12], a13: F[A13], a14: F[A14]): F[IProd[TL]] =
    d.divide14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(prodIso.to(_))
  def apply[F[_]](implicit d: Apply[F], a1: F[A1], a2: F[A2], a3: F[A3],
                  a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7],
                  a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11],
                  a12: F[A12], a13: F[A13], a14: F[A14]): F[IProd[TL]] =
    ApplyExt.apply14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(
      (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14) =>
        prodIso.from((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14)))
}
