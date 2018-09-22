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
  def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B]
  def mkDivide[B](f: B => Prod)(implicit a: Divide[F]): F[B]
  def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B]

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