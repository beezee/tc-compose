package bz

import iotaz.{Cop => ICop, Prod => IProd, TList, TNil}
import iotaz.TList.::
import scala.language.higherKinds
import scalaz.{Apply, Isomorphism => iso, \/}
import iso.<=>

abstract class TCCombine[F[_], TL <: TList, Cop, Prod](
    copIso: (ICop[TL] <=> Cop),
    prodIso: (IProd[TL] <=> Prod)) {
  def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B]
  def mkChooseI[B](f: B => ICop[TL])(implicit d: Decidable[F]): F[B] =
    mkChoose(f.andThen(copIso.to))
  def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B]
  def mkAltI[B](f: ICop[TL] => B)(implicit a: Alt[F]): F[B] =
    mkAlt(copIso.from.andThen(f))
  def mkDivide[B](f: B => Prod)(implicit a: Divide[F]): F[B]
  def mkDivideI[B](f: B => IProd[TL])(implicit a: Divide[F]): F[B] =
    mkDivide(f.andThen(prodIso.to))
  def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B]
  def mkApplyI[B](f: IProd[TL] => B)(implicit a: Apply[F]): F[B] =
    mkApply(prodIso.from.andThen(f))

  def chooseI(implicit d: Decidable[F]): F[ICop[TL]] = mkChoose(copIso.to(_))
  def choose(implicit d: Decidable[F]): F[Cop] = mkChoose(identity _)

  def altI(implicit a: Alt[F]): F[ICop[TL]] = mkAlt(copIso.from(_))
  def alt(implicit a: Alt[F]): F[Cop] = mkAlt(identity _)

  def divideI(implicit d: Divide[F]): F[IProd[TL]] = mkDivide(prodIso.to(_))
  def divide(implicit d: Divide[F]): F[Prod] = mkDivide(identity _)

  def applyI(implicit a: Apply[F]): F[IProd[TL]] = mkApply(prodIso.from(_))
  def apply(implicit a: Apply[F]): F[Prod] = mkApply(identity _)
}

trait TC {
  type TL <: TList
  type Cop
  type Prod
  def copIso: (ICop[TL] <=> Cop)
  def prodIso: (IProd[TL] <=> Prod)
  def inj[A](a: A)(implicit I: ICop.Inject[A, ICop[TL]]): ICop[TL] = I.inj(a)
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
  def combine[F[_]](implicit a1: F[A1], a2: F[A2]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose2(a1, a2)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly2(a1, a2)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide2(a1, a2)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply2(a1, a2)((a, b) => f((a, b)))
    }
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
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose3(a1, a2, a3)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly3(a1, a2, a3)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide3(a1, a2, a3)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply3(a1, a2, a3)((a, b, c) => f((a, b, c)))
    }
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
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose4(a1, a2, a3, a4)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly4(a1, a2, a3, a4)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide4(a1, a2, a3, a4)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply4(a1, a2, a3, a4)((i1, i2, i3, i4) => f((i1, i2, i3, i4)))
    }
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
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose5(a1, a2, a3, a4, a5)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly5(a1, a2, a3, a4, a5)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide5(a1, a2, a3, a4, a5)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply5(a1, a2, a3, a4, a5)((i1, i2, i3, i4, i5) => f((i1, i2, i3, i4, i5)))
    }
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
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose6(a1, a2, a3, a4, a5, a6)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly6(a1, a2, a3, a4, a5, a6)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide6(a1, a2, a3, a4, a5, a6)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply6(a1, a2, a3, a4, a5, a6)((i1, i2, i3, i4, i5, i6) => f((i1, i2, i3, i4, i5, i6)))
    }
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
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose7(a1, a2, a3, a4, a5, a6, a7)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly7(a1, a2, a3, a4, a5, a6, a7)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide7(a1, a2, a3, a4, a5, a6, a7)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply7(a1, a2, a3, a4, a5, a6, a7)(
          (i1, i2, i3, i4, i5, i6, i7) => f((i1, i2, i3, i4, i5, i6, i7)))
    }
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
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose8(a1, a2, a3, a4, a5, a6, a7, a8)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly8(a1, a2, a3, a4, a5, a6, a7, a8)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide8(a1, a2, a3, a4, a5, a6, a7, a8)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply8(a1, a2, a3, a4, a5, a6, a7, a8)(
          (i1, i2, i3, i4, i5, i6, i7, i8) => f((i1, i2, i3, i4, i5, i6, i7, i8)))
    }
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
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose9(a1, a2, a3, a4, a5, a6, a7, a8, a9)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly9(a1, a2, a3, a4, a5, a6, a7, a8, a9)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide9(a1, a2, a3, a4, a5, a6, a7, a8, a9)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply9(a1, a2, a3, a4, a5, a6, a7, a8, a9)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9) => f((i1, i2, i3, i4, i5, i6, i7, i8, i9)))
    }
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
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10) =>
            f((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10)))
    }
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
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11) =>
            f((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11)))
    }
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
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12) =>
            f((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12)))
    }
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
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        ApplyExt.apply13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13) =>
            f((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13)))
    }
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
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        ApplyExt.apply14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14) =>
            f((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14)))
    }
}

trait TC15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 ::
            A15 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             A15))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)
  val copIso = iso.IsoSet(
    Cops.to15(_: ICop[TL]),
    Cops.from15(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to15T(_: IProd[TL]),
    Prods.from15T(_: Prod))
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose15(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly15(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide15(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        ApplyExt.apply15(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15) =>
            f((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15)))
    }
}

trait TC16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 ::
            A15 :: A16 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             (A15 \/ A16)))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
  val copIso = iso.IsoSet(
    Cops.to16(_: ICop[TL]),
    Cops.from16(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to16T(_: IProd[TL]),
    Prods.from16T(_: Prod))
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose16(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly16(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide16(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        ApplyExt.apply16(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16) =>
            f((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16)))
    }
}

trait TC17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 ::
            A15 :: A16 :: A17 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             (A15 \/ (A16 \/ A17))))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)
  val copIso = iso.IsoSet(
    Cops.to17(_: ICop[TL]),
    Cops.from17(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to17T(_: IProd[TL]),
    Prods.from17T(_: Prod))
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16],
                    a17: F[A17]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose17(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly17(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide17(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        ApplyExt.apply17(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17) =>
            f((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17)))
    }
}

trait TC18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
           A15, A16, A17, A18] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 ::
            A15 :: A16 :: A17 :: A18 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, A18)
  val copIso = iso.IsoSet(
    Cops.to18(_: ICop[TL]),
    Cops.from18(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to18T(_: IProd[TL]),
    Prods.from18T(_: Prod))
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16],
                    a17: F[A17], a18: F[A18]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose18(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly18(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide18(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        ApplyExt.apply18(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18) =>
            f((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18)))
    }
}

trait TC19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
           A15, A16, A17, A18, A19] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 ::
            A15 :: A16 :: A17 :: A18 :: A19 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, A18, A19)
  val copIso = iso.IsoSet(
    Cops.to19(_: ICop[TL]),
    Cops.from19(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to19T(_: IProd[TL]),
    Prods.from19T(_: Prod))
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16],
                    a17: F[A17], a18: F[A18], a19: F[A19]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose19(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
          a15, a16, a17, a18, a19)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly19(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
          a15, a16, a17, a18, a19)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide19(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
          a15, a16, a17, a18, a19)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        ApplyExt.apply19(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
            a15, a16, a17, a18, a19)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19) =>
            f((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19)))
    }
}

trait TC20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
           A15, A16, A17, A18, A19, A20] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 ::
            A15 :: A16 :: A17 :: A18 :: A19 :: A20 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, A18, A19, A20)
  val copIso = iso.IsoSet(
    Cops.to20(_: ICop[TL]),
    Cops.from20(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to20T(_: IProd[TL]),
    Prods.from20T(_: Prod))
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16],
                    a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose20(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
          a15, a16, a17, a18, a19, a20)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly20(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
          a15, a16, a17, a18, a19, a20)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide20(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
          a15, a16, a17, a18, a19, a20)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        ApplyExt.apply20(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
            a15, a16, a17, a18, a19, a20)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14,
           i15, i16, i17, i18, i19, i20) =>
            f((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14,
               i15, i16, i17, i18, i19, i20)))
    }
}

trait TC21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
           A15, A16, A17, A18, A19, A20, A21] extends TC {
  type TL = A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 ::
            A15 :: A16 :: A17 :: A18 :: A19 :: A20 :: A21 :: TNil
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, A18, A19, A20, A21)
  val copIso = iso.IsoSet(
    Cops.to21(_: ICop[TL]),
    Cops.from21(_: Cop))
  val prodIso = iso.IsoSet(
    Prods.to21T(_: IProd[TL]),
    Prods.from21T(_: Prod))
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16],
                    a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20],
                    a21: F[A21]): TCCombine[F, TL, Cop, Prod] =
    new TCCombine[F, TL, Cop, Prod](copIso, prodIso) {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose21(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
          a15, a16, a17, a18, a19, a20, a21)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly21(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
          a15, a16, a17, a18, a19, a20, a21)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide21(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
          a15, a16, a17, a18, a19, a20, a21)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        ApplyExt.apply21(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
            a15, a16, a17, a18, a19, a20, a21)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14,
           i15, i16, i17, i18, i19, i20, i21) =>
            f((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14,
               i15, i16, i17, i18, i19, i20, i21)))
    }
}
