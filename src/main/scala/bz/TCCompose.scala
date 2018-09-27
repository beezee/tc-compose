package bz

import scala.language.higherKinds
import scalaz.{Apply, \/, Monoid, Semigroup}
import scalaz.syntax.either._

trait Inj[Cop, A] {
  def apply(a: A): Cop
}

object Inj {

  trait Aux[Cop] {
    type Out[A] = Inj[Cop, A]
  }

  def instance[A, B](ab: A => B): Inj[B, A] = new Inj[B, A] {
    def apply(a: A): B = ab(a)
  }

  implicit def decidableInj[Cop]: Decidable[Aux[Cop]#Out] =
    new Decidable[Aux[Cop]#Out] {
      type I[A] = Aux[Cop]#Out[A]
      def choose2[Z, A1, A2](a1: =>I[A1], a2: =>I[A2])(f: Z => (A1 \/ A2)): I[Z] =
        new Inj[Cop, Z] {
          def apply(z: Z): Cop = f(z).fold(a1.apply(_), a2.apply(_))
        }
    }

  implicit def divideInj[Prod](implicit S: Semigroup[Prod]): Divide[Aux[Prod]#Out] =
    new Divide[Aux[Prod]#Out] {
      type I[A] = Aux[Prod]#Out[A]
      def contramap[A, B](ia: I[A])(f: B => A): I[B] = new Inj[Prod, B] {
        def apply(b: B): Prod = ia(f(b))
      }
      def divide2[A1, A2, Z](a1: =>I[A1], a2: =>I[A2])(f: Z => (A1, A2)): I[Z] =
        new Inj[Prod, Z] {
          def apply(z: Z): Prod = {
            val (i1, i2) = f(z)
            S.append(a1(i1), a2(i2))
          }
        }
    }
}

abstract class TCCombine[F[_], Cop, Prod] {
  def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B]
  def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B]
  def mkDivide[B](f: B => Prod)(implicit a: Divide[F]): F[B]
  def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B]

  def choose(implicit d: Decidable[F]): F[Cop] = mkChoose(identity _)

  def alt(implicit a: Alt[F]): F[Cop] = mkAlt(identity _)

  def divide(implicit d: Divide[F]): F[Prod] = mkDivide(identity _)

  def apply(implicit a: Apply[F]): F[Prod] = mkApply(identity _)
}

trait TC {
  type Cop
  type Prod
  val injEv: Inj[Cop, Cop]
  def inj[A](a: A)(implicit inj: Inj[Cop, A]): Cop = inj(a)
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod]
  def lift[A](a: A)(implicit inj: Inj[Prod, A]): Prod = inj(a)
}

object TC {
  def combine[A1]: TC1[A1] = new TC1[A1] {}
  def combineK[F[_], A1]: TC1[F[A1]] = new TC1[F[A1]] {}
  def combine[A1, A2]: TC2[A1, A2] = new TC2[A1, A2] {}
  def combineK[F[_], A1, A2]: TC2[F[A1], F[A2]] = new TC2[F[A1], F[A2]] {}
  def combine[A1, A2, A3]: TC3[A1, A2, A3] = new TC3[A1, A2, A3] {}
  def combineK[F[_], A1, A2, A3]: TC3[F[A1], F[A2], F[A3]] =
    new TC3[F[A1], F[A2], F[A3]] {}
  def combine[A1, A2, A3, A4]: TC4[A1, A2, A3, A4] = new TC4[A1, A2, A3, A4] {}
  def combineK[F[_], A1, A2, A3, A4]: TC4[F[A1], F[A2], F[A3], F[A4]] =
    new TC4[F[A1], F[A2], F[A3], F[A4]] {}
  def combine[A1, A2, A3, A4, A5]: TC5[A1, A2, A3, A4, A5] =
    new TC5[A1, A2, A3, A4, A5] {}
  def combineK[F[_], A1, A2, A3, A4, A5]: TC5[F[A1], F[A2], F[A3], F[A4], F[A5]] =
    new TC5[F[A1], F[A2], F[A3], F[A4], F[A5]] {}
  def combine[A1, A2, A3, A4, A5, A6]: TC6[A1, A2, A3, A4, A5, A6] =
    new TC6[A1, A2, A3, A4, A5, A6] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6]: TC6[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6]] =
    new TC6[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7]:
  TC7[A1, A2, A3, A4, A5, A6, A7] =
    new TC7[A1, A2, A3, A4, A5, A6, A7] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7]:
  TC7[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7]] =
    new TC7[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8]:
  TC8[A1, A2, A3, A4, A5, A6, A7, A8] =
    new TC8[A1, A2, A3, A4, A5, A6, A7, A8] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8]:
  TC8[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8]] =
    new TC8[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9]:
  TC9[A1, A2, A3, A4, A5, A6, A7, A8, A9] =
    new TC9[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]:
  TC9[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]] =
    new TC9[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]:
  TC10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] =
    new TC10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]:
  TC10[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10]] =
    new TC10[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]:
  TC11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
    new TC11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]:
  TC11[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]] =
    new TC11[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]:
  TC12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
    new TC12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]:
  TC12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]] =
    new TC12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]:
  TC13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
    new TC13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
             A11, A12, A13] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]:
  TC13[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13]] =
    new TC13[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10],
             F[A11], F[A12], F[A13]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]:
  TC14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
    new TC14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
             A11, A12, A13, A14] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]:
  TC14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9],
       F[A10], F[A11], F[A12], F[A13], F[A14]] =
    new TC14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10],
             F[A11], F[A12], F[A13], F[A14]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
              A11, A12, A13, A14, A15]:
  TC15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
    new TC15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
             A11, A12, A13, A14, A15] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
               A11, A12, A13, A14, A15]:
  TC15[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9],
       F[A10], F[A11], F[A12], F[A13], F[A14], F[A15]] =
    new TC15[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10],
             F[A11], F[A12], F[A13], F[A14], F[A15]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
              A11, A12, A13, A14, A15, A16]:
  TC16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
    new TC16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
             A11, A12, A13, A14, A15, A16] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
               A11, A12, A13, A14, A15, A16]:
  TC16[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9],
       F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16]] =
    new TC16[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10],
             F[A11], F[A12], F[A13], F[A14], F[A15], F[A16]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
              A11, A12, A13, A14, A15, A16, A17]:
  TC17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
    new TC17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
             A11, A12, A13, A14, A15, A16, A17] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
               A11, A12, A13, A14, A15, A16, A17]:
  TC17[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9],
       F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17]] =
    new TC17[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10],
             F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
              A11, A12, A13, A14, A15, A16, A17, A18]:
  TC18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
    new TC18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
             A11, A12, A13, A14, A15, A16, A17, A18] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
               A11, A12, A13, A14, A15, A16, A17, A18]:
  TC18[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9],
       F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18]] =
    new TC18[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10],
             F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
              A11, A12, A13, A14, A15, A16, A17, A18, A19]:
  TC19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
    new TC19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
             A11, A12, A13, A14, A15, A16, A17, A18, A19] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
               A11, A12, A13, A14, A15, A16, A17, A18, A19]:
  TC19[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9],
       F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19]] =
    new TC19[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10],
             F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
              A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]:
  TC20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
       A15, A16, A17, A18, A19, A20] =
    new TC20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
             A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
               A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]:
  TC20[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9],
       F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20]] =
    new TC20[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10],
             F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
              A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]:
  TC21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
       A15, A16, A17, A18, A19, A20, A21] =
    new TC21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
             A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
               A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]:
  TC21[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9],
       F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20], F[A21]] =
    new TC21[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10],
             F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20], F[A21]] {}
  def combine[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
              A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]:
  TC22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
       A15, A16, A17, A18, A19, A20, A21, A22] =
    new TC22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
             A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}
  def combineK[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
               A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]:
  TC22[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9],
       F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17],
       F[A18], F[A19], F[A20], F[A21], F[A22]] =
    new TC22[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10],
             F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18],
             F[A19], F[A20], F[A21], F[A22]] {}
}

trait TC1[A1] extends TC {
  type Cop = A1
  type Prod = A1
  val injEv = Inj.instance(identity[A1] _)
  def liftEv(implicit M: Monoid[Prod]) = injEv
}

trait TC2[A1, A2] extends TC {
  type Cop = (A1 \/ A2)
  type Prod = (A1, A2)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose2(a1, a2)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly2(a1, a2)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide2(a1, a2)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply2(a1, a2)((a, b) => f((a, b)))
    }
  implicit val inja1: Inj[Cop, A1] = Inj.instance(_.left[A2])
  implicit val inja2: Inj[Cop, A2] = Inj.instance(_.right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2) = M.zero
    Inj.instance((_, a2))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _) = M.zero
    Inj.instance((a1, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

}

trait TC3[A1, A2, A3] extends TC {
  type Cop = (A1 \/ (A2 \/ A3))
  type Prod = (A1, A2, A3)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose3(a1, a2, a3)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly3(a1, a2, a3)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide3(a1, a2, a3)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply3(a1, a2, a3)((a, b, c) => f((a, b, c)))
    }
  implicit val inja1: Inj[Cop, A1] = Inj.instance(_.left[A2 \/ A3])
  implicit val inja2: Inj[Cop, A2] = Inj.instance(_.left[A3].right[A1])
  implicit val inja3: Inj[Cop, A3] = Inj.instance(_.right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3) = M.zero
    Inj.instance((_, a2, a3))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3) = M.zero
    Inj.instance((a1, _, a3))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _) = M.zero
    Inj.instance((a1, a2, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC4[A1, A2, A3, A4] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ A4)))
  type Prod = (A1, A2, A3, A4)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose4(a1, a2, a3, a4)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly4(a1, a2, a3, a4)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide4(a1, a2, a3, a4)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply4(a1, a2, a3, a4)((i1, i2, i3, i4) => f((i1, i2, i3, i4)))
    }
  implicit val inja1: Inj[Cop, A1] = Inj.instance(_.left[A2 \/ (A3 \/ A4)])
  implicit val inja2: Inj[Cop, A2] = Inj.instance(_.left[A3 \/ A4].right[A1])
  implicit val inja3: Inj[Cop, A3] = Inj.instance(_.left[A4].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] = Inj.instance(_.right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4) = M.zero
    Inj.instance((_, a2, a3, a4))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4) = M.zero
    Inj.instance((a1, _, a3, a4))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4) = M.zero
    Inj.instance((a1, a2, _, a4))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _) = M.zero
    Inj.instance((a1, a2, a3, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC5[A1, A2, A3, A4, A5] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ A5))))
  type Prod = (A1, A2, A3, A4, A5)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose5(a1, a2, a3, a4, a5)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly5(a1, a2, a3, a4, a5)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide5(a1, a2, a3, a4, a5)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply5(a1, a2, a3, a4, a5)((i1, i2, i3, i4, i5) => f((i1, i2, i3, i4, i5)))
    }
  implicit val inja1: Inj[Cop, A1] = Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ A5))])
  implicit val inja2: Inj[Cop, A2] = Inj.instance(_.left[A3 \/ (A4 \/ A5)].right[A1])
  implicit val inja3: Inj[Cop, A3] = Inj.instance(_.left[A4 \/ A5].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] = Inj.instance(_.left[A5].right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] = Inj.instance(_.right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5) = M.zero
    Inj.instance((_, a2, a3, a4, a5))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5) = M.zero
    Inj.instance((a1, _, a3, a4, a5))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5) = M.zero
    Inj.instance((a1, a2, _, a4, a5))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5) = M.zero
    Inj.instance((a1, a2, a3, _, a5))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _) = M.zero
    Inj.instance((a1, a2, a3, a4, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC6[A1, A2, A3, A4, A5, A6] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6)))))
  type Prod = (A1, A2, A3, A4, A5, A6)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose6(a1, a2, a3, a4, a5, a6)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly6(a1, a2, a3, a4, a5, a6)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide6(a1, a2, a3, a4, a5, a6)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        a.apply6(a1, a2, a3, a4, a5, a6)((i1, i2, i3, i4, i5, i6) => f((i1, i2, i3, i4, i5, i6)))
    }
  implicit val inja1: Inj[Cop, A1] = Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ A6)))])
  implicit val inja2: Inj[Cop, A2] = Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ A6))].right[A1])
  implicit val inja3: Inj[Cop, A3] = Inj.instance(_.left[A4 \/ (A5 \/ A6)].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] = Inj.instance(_.left[A5 \/ A6].right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] = Inj.instance(_.left[A6].right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] = Inj.instance(_.right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC7[A1, A2, A3, A4, A5, A6, A7] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ A7))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ A7)].right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ A7].right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC8[A1, A2, A3, A4, A5, A6, A7, A8] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8)))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8)))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/ A8)))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/ A8))].right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/ A8)].right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/ A8].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC9[A1, A2, A3, A4, A5, A6, A7, A8, A9] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))].right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/ (A8 \/ A9))].right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/ (A8 \/ A9)].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ A9].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9].right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.right[A8].right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))].right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))].right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/ (A8 \/ (A9 \/ A10))].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ (A9 \/ A10)].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9 \/ A10].right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.left[A10].right[A8].right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja10: Inj[Cop, A10] =
    Inj.instance(_.right[A9].right[A8].right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9, a10) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9, a10))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9, a10) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9, a10))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9, a10) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9, a10))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9, a10) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9, a10))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9, a10) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9, a10))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9, a10) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9, a10))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9, a10) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9, a10))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9, a10) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9, a10))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _, a10) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _, a10))
  }
  implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, A10] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ A11))))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ A11)))))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ A11))))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ A11)))))].right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ A11))))].right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/
      (A8 \/ (A9 \/ (A10 \/ A11)))].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ (A9 \/ (A10 \/ A11))].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9 \/ (A10 \/ A11)].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.left[A10 \/ A11].right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja10: Inj[Cop, A10] =
    Inj.instance(_.left[A11].right[A9].right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja11: Inj[Cop, A11] =
    Inj.instance(_.right[A10].right[A9].right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11))
  }
  implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, A10] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11))
  }
  implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, A11] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))].right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))].right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9 \/ (A10 \/ (A11 \/ A12))].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.left[A10 \/ (A11 \/ A12)].right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja10: Inj[Cop, A10] =
    Inj.instance(_.left[A11 \/ A12].right[A9].right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja11: Inj[Cop, A11] =
    Inj.instance(_.left[A12].right[A10].right[A9].right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja12: Inj[Cop, A12] =
    Inj.instance(_.right[A11].right[A10].right[A9].right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12))
  }
  implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, A10] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12))
  }
  implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, A11] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12))
  }
  implicit def lifta12(implicit M: Monoid[Prod]): Inj[Prod, A12] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))].right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))].right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.left[A10 \/ (A11 \/ (A12 \/ A13))].right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja10: Inj[Cop, A10] =
    Inj.instance(_.left[A11 \/ (A12 \/ A13)].right[A9].right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja11: Inj[Cop, A11] =
    Inj.instance(_.left[(A12 \/ A13)].right[A10].right[A9].right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja12: Inj[Cop, A12] =
    Inj.instance(_.left[A13].right[A11].right[A10].right[A9].right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja13: Inj[Cop, A13] =
    Inj.instance(_.right[A12].right[A11].right[A10].right[A9].right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13))
  }
  implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, A10] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13))
  }
  implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, A11] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13))
  }
  implicit def lifta12(implicit M: Monoid[Prod]): Inj[Prod, A12] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13))
  }
  implicit def lifta13(implicit M: Monoid[Prod]): Inj[Prod, A13] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))]
      .right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))]
      .right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))]
      .right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.left[A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))]
      .right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja10: Inj[Cop, A10] =
    Inj.instance(_.left[A11 \/ (A12 \/ (A13 \/ A14))].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja11: Inj[Cop, A11] =
    Inj.instance(_.left[(A12 \/ (A13 \/ A14))].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja12: Inj[Cop, A12] =
    Inj.instance(_.left[(A13 \/ A14)].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja13: Inj[Cop, A13] =
    Inj.instance(_.left[A14].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja14: Inj[Cop, A14] =
    Inj.instance(_.right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14))
  }
  implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, A10] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14))
  }
  implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, A11] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14))
  }
  implicit def lifta12(implicit M: Monoid[Prod]): Inj[Prod, A12] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14))
  }
  implicit def lifta13(implicit M: Monoid[Prod]): Inj[Prod, A13] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14))
  }
  implicit def lifta14(implicit M: Monoid[Prod]): Inj[Prod, A14] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             A15))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/A15))))))))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/A15)))))))))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/A15))))))))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/A15)))))))))]
      .right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/A15))))))))]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/A15)))))))]
      .right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/A15))))))]
      .right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/A15)))))]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.left[A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/A15))))]
      .right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja10: Inj[Cop, A10] =
    Inj.instance(_.left[A11 \/ (A12 \/ (A13 \/ (A14 \/A15)))].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja11: Inj[Cop, A11] =
    Inj.instance(_.left[(A12 \/ (A13 \/ (A14 \/A15)))].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja12: Inj[Cop, A12] =
    Inj.instance(_.left[(A13 \/ (A14 \/A15))].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja13: Inj[Cop, A13] =
    Inj.instance(_.left[(A14 \/A15)].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja14: Inj[Cop, A14] =
    Inj.instance(_.left[A15].right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja15: Inj[Cop, A15] =
    Inj.instance(_.right[A14].right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14, a15) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14, a15))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14, a15) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14, a15))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14, a15) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14, a15))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14, a15) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14, a15))
  }
  implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, A10] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14, a15) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14, a15))
  }
  implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, A11] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14, a15) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14, a15))
  }
  implicit def lifta12(implicit M: Monoid[Prod]): Inj[Prod, A12] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14, a15) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14, a15))
  }
  implicit def lifta13(implicit M: Monoid[Prod]): Inj[Prod, A13] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14, a15) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14, a15))
  }
  implicit def lifta14(implicit M: Monoid[Prod]): Inj[Prod, A14] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _, a15) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _, a15))
  }
  implicit def lifta15(implicit M: Monoid[Prod]): Inj[Prod, A15] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             (A15 \/ A16)))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/A16)))))))))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/A16))))))))))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/A16)))))))))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/A16))))))))))]
      .right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/A16)))))))))]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/A16))))))))]
      .right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/A16)))))))]
      .right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/A16))))))]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.left[A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/A16)))))]
      .right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja10: Inj[Cop, A10] =
    Inj.instance(_.left[A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/A16))))].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja11: Inj[Cop, A11] =
    Inj.instance(_.left[(A12 \/ (A13 \/ (A14 \/(A15 \/A16))))].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja12: Inj[Cop, A12] =
    Inj.instance(_.left[(A13 \/ (A14 \/(A15 \/A16)))].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja13: Inj[Cop, A13] =
    Inj.instance(_.left[(A14 \/(A15 \/A16))].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja14: Inj[Cop, A14] =
    Inj.instance(_.left[(A15 \/A16)].right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja15: Inj[Cop, A15] =
    Inj.instance(_.left[A16].right[A14].right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja16: Inj[Cop, A16] =
    Inj.instance(_.right[A15].right[A14].right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14, a15, a16) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14, a15, a16))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14, a15, a16) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14, a15, a16))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14, a15, a16) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14, a15, a16))
  }
  implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, A10] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14, a15, a16) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14, a15, a16))
  }
  implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, A11] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14, a15, a16) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14, a15, a16))
  }
  implicit def lifta12(implicit M: Monoid[Prod]): Inj[Prod, A12] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14, a15, a16) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14, a15, a16))
  }
  implicit def lifta13(implicit M: Monoid[Prod]): Inj[Prod, A13] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14, a15, a16) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14, a15, a16))
  }
  implicit def lifta14(implicit M: Monoid[Prod]): Inj[Prod, A14] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _, a15, a16) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _, a15, a16))
  }
  implicit def lifta15(implicit M: Monoid[Prod]): Inj[Prod, A15] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16))
  }
  implicit def lifta16(implicit M: Monoid[Prod]): Inj[Prod, A16] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             (A15 \/ (A16 \/ A17))))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16],
                    a17: F[A17]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ A17))))))))))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ A17)))))))))))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ A17))))))))))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ A17)))))))))))]
      .right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ A17))))))))))]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ A17)))))))))]
      .right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
    (A15 \/(A16 \/ A17))))))))]
      .right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
    (A15 \/(A16 \/ A17)))))))]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.left[A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/(A16 \/ A17))))))]
      .right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja10: Inj[Cop, A10] =
    Inj.instance(_.left[A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/(A16 \/ A17)))))]
      .right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja11: Inj[Cop, A11] =
    Inj.instance(_.left[(A12 \/ (A13 \/ (A14 \/(A15 \/(A16 \/ A17)))))]
      .right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja12: Inj[Cop, A12] =
    Inj.instance(_.left[(A13 \/ (A14 \/(A15 \/(A16 \/ A17))))]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja13: Inj[Cop, A13] =
    Inj.instance(_.left[(A14 \/(A15 \/(A16 \/ A17)))]
      .right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja14: Inj[Cop, A14] =
    Inj.instance(_.left[(A15 \/(A16 \/ A17))]
      .right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja15: Inj[Cop, A15] =
    Inj.instance(_.left[(A16 \/ A17)]
      .right[A14].right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja16: Inj[Cop, A16] =
    Inj.instance(_.left[A17].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja17: Inj[Cop, A17] =
    Inj.instance(_.right[A16].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14, a15, a16, a17) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14, a15, a16, a17))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14, a15, a16, a17) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14, a15, a16, a17))
  }
  implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, A10] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14, a15, a16, a17) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14, a15, a16, a17))
  }
  implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, A11] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14, a15, a16, a17) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14, a15, a16, a17))
  }
  implicit def lifta12(implicit M: Monoid[Prod]): Inj[Prod, A12] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14, a15, a16, a17) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14, a15, a16, a17))
  }
  implicit def lifta13(implicit M: Monoid[Prod]): Inj[Prod, A13] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14, a15, a16, a17) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14, a15, a16, a17))
  }
  implicit def lifta14(implicit M: Monoid[Prod]): Inj[Prod, A14] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _, a15, a16, a17) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _, a15, a16, a17))
  }
  implicit def lifta15(implicit M: Monoid[Prod]): Inj[Prod, A15] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16, a17) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16, a17))
  }
  implicit def lifta16(implicit M: Monoid[Prod]): Inj[Prod, A16] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, _, a17) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, _, a17))
  }
  implicit def lifta17(implicit M: Monoid[Prod]): Inj[Prod, A17] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
           A15, A16, A17, A18] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, A18)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16],
                    a17: F[A17], a18: F[A18]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ A18)))))))))))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ A18))))))))))))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ A18)))))))))))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ A18))))))))))))]
      .right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ A18)))))))))))]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ A18))))))))))]
      .right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
    (A15 \/(A16 \/ (A17 \/ A18)))))))))]
      .right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
    (A15 \/(A16 \/ (A17 \/ A18))))))))]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.left[A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/(A16 \/ (A17 \/ A18)))))))]
      .right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja10: Inj[Cop, A10] =
    Inj.instance(_.left[A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/(A16 \/ (A17 \/ A18))))))]
      .right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja11: Inj[Cop, A11] =
    Inj.instance(_.left[(A12 \/ (A13 \/ (A14 \/(A15 \/(A16 \/ (A17 \/ A18))))))]
      .right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja12: Inj[Cop, A12] =
    Inj.instance(_.left[(A13 \/ (A14 \/(A15 \/(A16 \/ (A17 \/ A18)))))]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja13: Inj[Cop, A13] =
    Inj.instance(_.left[(A14 \/(A15 \/(A16 \/ (A17 \/ A18))))]
      .right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja14: Inj[Cop, A14] =
    Inj.instance(_.left[(A15 \/(A16 \/ (A17 \/ A18)))]
      .right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja15: Inj[Cop, A15] =
    Inj.instance(_.left[(A16 \/ (A17 \/ A18))]
      .right[A14].right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja16: Inj[Cop, A16] =
    Inj.instance(_.left[(A17 \/ A18)].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja17: Inj[Cop, A17] =
    Inj.instance(_.left[A18].right[A16].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja18: Inj[Cop, A18] =
    Inj.instance(_.right[A17].right[A16].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14, a15, a16, a17, a18) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14, a15, a16, a17, a18))
  }
  implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, A10] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14, a15, a16, a17, a18) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14, a15, a16, a17, a18))
  }
  implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, A11] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14, a15, a16, a17, a18) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14, a15, a16, a17, a18))
  }
  implicit def lifta12(implicit M: Monoid[Prod]): Inj[Prod, A12] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14, a15, a16, a17, a18) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14, a15, a16, a17, a18))
  }
  implicit def lifta13(implicit M: Monoid[Prod]): Inj[Prod, A13] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14, a15, a16, a17, a18) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14, a15, a16, a17, a18))
  }
  implicit def lifta14(implicit M: Monoid[Prod]): Inj[Prod, A14] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _, a15, a16, a17, a18) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _, a15, a16, a17, a18))
  }
  implicit def lifta15(implicit M: Monoid[Prod]): Inj[Prod, A15] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16, a17, a18) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16, a17, a18))
  }
  implicit def lifta16(implicit M: Monoid[Prod]): Inj[Prod, A16] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, _, a17, a18) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, _, a17, a18))
  }
  implicit def lifta17(implicit M: Monoid[Prod]): Inj[Prod, A17] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, _, a18) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, _, a18))
  }
  implicit def lifta18(implicit M: Monoid[Prod]): Inj[Prod, A18] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
           A15, A16, A17, A18, A19] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, A18, A19)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16],
                    a17: F[A17], a18: F[A18], a19: F[A19]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ A19))))))))))))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ A19)))))))))))))]
      .right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ A19))))))))))))]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ A19)))))))))))]
      .right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
    (A15 \/(A16 \/ (A17 \/ (A18 \/ A19))))))))))]
      .right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
    (A15 \/(A16 \/ (A17 \/ (A18 \/ A19)))))))))]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.left[A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ A19))))))))]
      .right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja10: Inj[Cop, A10] =
    Inj.instance(_.left[A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ A19)))))))]
      .right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja11: Inj[Cop, A11] =
    Inj.instance(_.left[(A12 \/ (A13 \/ (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ A19)))))))]
      .right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja12: Inj[Cop, A12] =
    Inj.instance(_.left[(A13 \/ (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ A19))))))]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja13: Inj[Cop, A13] =
    Inj.instance(_.left[(A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ A19)))))]
      .right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja14: Inj[Cop, A14] =
    Inj.instance(_.left[(A15 \/(A16 \/ (A17 \/ (A18 \/ A19))))]
      .right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja15: Inj[Cop, A15] =
    Inj.instance(_.left[(A16 \/ (A17 \/ (A18 \/ A19)))]
      .right[A14].right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja16: Inj[Cop, A16] =
    Inj.instance(_.left[(A17 \/ (A18 \/ A19))].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja17: Inj[Cop, A17] =
    Inj.instance(_.left[(A18 \/ A19)].right[A16].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja18: Inj[Cop, A18] =
    Inj.instance(_.left[A19].right[A17].right[A16].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja19: Inj[Cop, A19] =
    Inj.instance(_.right[A18].right[A17].right[A16].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19))
  }
  implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, A10] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14, a15, a16, a17, a18, a19))
  }
  implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, A11] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14, a15, a16, a17, a18, a19))
  }
  implicit def lifta12(implicit M: Monoid[Prod]): Inj[Prod, A12] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14, a15, a16, a17, a18, a19))
  }
  implicit def lifta13(implicit M: Monoid[Prod]): Inj[Prod, A13] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14, a15, a16, a17, a18, a19))
  }
  implicit def lifta14(implicit M: Monoid[Prod]): Inj[Prod, A14] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _, a15, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _, a15, a16, a17, a18, a19))
  }
  implicit def lifta15(implicit M: Monoid[Prod]): Inj[Prod, A15] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16, a17, a18, a19))
  }
  implicit def lifta16(implicit M: Monoid[Prod]): Inj[Prod, A16] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, _, a17, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, _, a17, a18, a19))
  }
  implicit def lifta17(implicit M: Monoid[Prod]): Inj[Prod, A17] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, _, a18, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, _, a18, a19))
  }
  implicit def lifta18(implicit M: Monoid[Prod]): Inj[Prod, A18] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, _, a19) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, _, a19))
  }
  implicit def lifta19(implicit M: Monoid[Prod]): Inj[Prod, A19] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
           A15, A16, A17, A18, A19, A20] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, A18, A19, A20)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16],
                    a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))]
      .right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))]
      .right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
    (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))]
      .right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
    (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.left[A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))]
      .right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja10: Inj[Cop, A10] =
    Inj.instance(_.left[A11 \/ (A12 \/ (A13 \/ (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))]
      .right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja11: Inj[Cop, A11] =
    Inj.instance(_.left[(A12 \/ (A13 \/ (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))]
      .right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja12: Inj[Cop, A12] =
    Inj.instance(_.left[(A13 \/ (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja13: Inj[Cop, A13] =
    Inj.instance(_.left[(A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))]
      .right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja14: Inj[Cop, A14] =
    Inj.instance(_.left[(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))]
      .right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja15: Inj[Cop, A15] =
    Inj.instance(_.left[(A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))]
      .right[A14].right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja16: Inj[Cop, A16] =
    Inj.instance(_.left[(A17 \/ (A18 \/ (A19 \/ A20)))].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja17: Inj[Cop, A17] =
    Inj.instance(_.left[(A18 \/ (A19 \/ A20))].right[A16].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja18: Inj[Cop, A18] =
    Inj.instance(_.left[(A19 \/ A20)].right[A17].right[A16].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja19: Inj[Cop, A19] =
    Inj.instance(_.left[A20].right[A18].right[A17].right[A16].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja20: Inj[Cop, A20] =
    Inj.instance(_.right[A19].right[A18].right[A17].right[A16].right[A15].right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, A10] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, A11] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta12(implicit M: Monoid[Prod]): Inj[Prod, A12] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta13(implicit M: Monoid[Prod]): Inj[Prod, A13] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta14(implicit M: Monoid[Prod]): Inj[Prod, A14] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _, a15, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _, a15, a16, a17, a18, a19, a20))
  }
  implicit def lifta15(implicit M: Monoid[Prod]): Inj[Prod, A15] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16, a17, a18, a19, a20))
  }
  implicit def lifta16(implicit M: Monoid[Prod]): Inj[Prod, A16] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, _, a17, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, _, a17, a18, a19, a20))
  }
  implicit def lifta17(implicit M: Monoid[Prod]): Inj[Prod, A17] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, _, a18, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, _, a18, a19, a20))
  }
  implicit def lifta18(implicit M: Monoid[Prod]): Inj[Prod, A18] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, _, a19, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, _, a19, a20))
  }
  implicit def lifta19(implicit M: Monoid[Prod]): Inj[Prod, A19] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, _, a20) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, _, a20))
  }
  implicit def lifta20(implicit M: Monoid[Prod]): Inj[Prod, A20] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
           A15, A16, A17, A18, A19, A20, A21] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, A18, A19, A20, A21)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16],
                    a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20],
                    a21: F[A21]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
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
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))]
      .right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))]
      .right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
    (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))]
      .right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
    (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.left[A10 \/ (A11 \/ (A12 \/ (A13 \/
    (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))]
      .right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja10: Inj[Cop, A10] =
    Inj.instance(_.left[A11 \/ (A12 \/ (A13 \/
    (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))]
      .right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja11: Inj[Cop, A11] =
    Inj.instance(_.left[(A12 \/ (A13 \/
    (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))]
      .right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja12: Inj[Cop, A12] =
    Inj.instance(_.left[(A13 \/
    (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja13: Inj[Cop, A13] =
    Inj.instance(_.left[(A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))]
      .right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja14: Inj[Cop, A14] =
    Inj.instance(_.left[(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))]
      .right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja15: Inj[Cop, A15] =
    Inj.instance(_.left[(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))]
      .right[A14].right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja16: Inj[Cop, A16] =
    Inj.instance(_.left[(A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))].right[A15]
      .right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja17: Inj[Cop, A17] =
    Inj.instance(_.left[(A18 \/ (A19 \/ (A20 \/ A21)))].right[A16].right[A15]
      .right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja18: Inj[Cop, A18] =
    Inj.instance(_.left[(A19 \/ (A20 \/ A21))].right[A17].right[A16].right[A15]
      .right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja19: Inj[Cop, A19] =
    Inj.instance(_.left[(A20 \/ A21)].right[A18].right[A17].right[A16].right[A15]
      .right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja20: Inj[Cop, A20] =
    Inj.instance(_.left[A21].right[A19].right[A18].right[A17].right[A16].right[A15]
      .right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja21: Inj[Cop, A21] =
    Inj.instance(_.right[A20].right[A19].right[A18].right[A17].right[A16].right[A15]
      .right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, A10] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, A11] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta12(implicit M: Monoid[Prod]): Inj[Prod, A12] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14,
      a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14,
      a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta13(implicit M: Monoid[Prod]): Inj[Prod, A13] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14,
			a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14,
			a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta14(implicit M: Monoid[Prod]): Inj[Prod, A14] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _,
			a15, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _,
			a15, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta15(implicit M: Monoid[Prod]): Inj[Prod, A15] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16, a17, a18, a19, a20, a21))
  }
  implicit def lifta16(implicit M: Monoid[Prod]): Inj[Prod, A16] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, _, a17, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, _, a17, a18, a19, a20, a21))
  }
  implicit def lifta17(implicit M: Monoid[Prod]): Inj[Prod, A17] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, _, a18, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, _, a18, a19, a20, a21))
  }
  implicit def lifta18(implicit M: Monoid[Prod]): Inj[Prod, A18] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, _, a19, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, _, a19, a20, a21))
  }
  implicit def lifta19(implicit M: Monoid[Prod]): Inj[Prod, A19] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, _, a20, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, _, a20, a21))
  }
  implicit def lifta20(implicit M: Monoid[Prod]): Inj[Prod, A20] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, a19, _, a21) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, a19, _, a21))
  }
  implicit def lifta21(implicit M: Monoid[Prod]): Inj[Prod, A21] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, a19, a20, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, a19, a20, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}

trait TC22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
           A15, A16, A17, A18, A19, A20, A21, A22] extends TC {
  type Cop = (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
             (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
             (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))
  type Prod = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
               A15, A16, A17, A18, A19, A20, A21, A22)
  def combine[F[_]](implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4],
                    a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8],
                    a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12],
                    a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16],
                    a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20],
                    a21: F[A21], a22: F[A22]): TCCombine[F, Cop, Prod] =
    new TCCombine[F, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[F]): F[B] =
        d.choose22(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
          a15, a16, a17, a18, a19, a20, a21, a22)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[F]): F[B] =
        a.altly22(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
          a15, a16, a17, a18, a19, a20, a21, a22)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[F]): F[B] =
        d.divide22(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
          a15, a16, a17, a18, a19, a20, a21, a22)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[F]): F[B] =
        ApplyExt.apply22(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
            a15, a16, a17, a18, a19, a20, a21, a22)(
          (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14,
           i15, i16, i17, i18, i19, i20, i21, i22) =>
            f((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14,
               i15, i16, i17, i18, i19, i20, i21, i22)))
    }
  implicit val inja1: Inj[Cop, A1] =
    Inj.instance(_.left[A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))])
  implicit val inja2: Inj[Cop, A2] =
    Inj.instance(_.left[A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))].right[A1])
  implicit val inja3: Inj[Cop, A3] =
    Inj.instance(_.left[A4 \/ (A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))].right[A2].right[A1])
  implicit val inja4: Inj[Cop, A4] =
    Inj.instance(_.left[A5 \/ (A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))]
      .right[A3].right[A2].right[A1])
  implicit val inja5: Inj[Cop, A5] =
    Inj.instance(_.left[A6 \/ (A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja6: Inj[Cop, A6] =
    Inj.instance(_.left[A7 \/
      (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
      (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))]
      .right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja7: Inj[Cop, A7] =
    Inj.instance(_.left[A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
    (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))]
      .right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja8: Inj[Cop, A8] =
    Inj.instance(_.left[A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
    (A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja9: Inj[Cop, A9] =
    Inj.instance(_.left[A10 \/ (A11 \/ (A12 \/ (A13 \/
    (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))]
      .right[A8].right[A7].right[A6].right[A5]
      .right[A4].right[A3].right[A2].right[A1])
  implicit val inja10: Inj[Cop, A10] =
    Inj.instance(_.left[A11 \/ (A12 \/ (A13 \/
    (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))]
      .right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja11: Inj[Cop, A11] =
    Inj.instance(_.left[(A12 \/ (A13 \/
    (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))]
      .right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja12: Inj[Cop, A12] =
    Inj.instance(_.left[(A13 \/
    (A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja13: Inj[Cop, A13] =
    Inj.instance(_.left[(A14 \/(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))]
      .right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja14: Inj[Cop, A14] =
    Inj.instance(_.left[(A15 \/(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))]
      .right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja15: Inj[Cop, A15] =
    Inj.instance(_.left[(A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))]
      .right[A14].right[A13].right[A12].right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja16: Inj[Cop, A16] =
    Inj.instance(_.left[(A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))].right[A15]
      .right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja17: Inj[Cop, A17] =
    Inj.instance(_.left[(A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))].right[A16].right[A15]
      .right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja18: Inj[Cop, A18] =
    Inj.instance(_.left[(A19 \/ (A20 \/ (A21 \/ A22)))].right[A17].right[A16].right[A15]
      .right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja19: Inj[Cop, A19] =
    Inj.instance(_.left[(A20 \/ (A21 \/ A22))].right[A18].right[A17].right[A16].right[A15]
      .right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja20: Inj[Cop, A20] =
    Inj.instance(_.left[(A21 \/ A22)].right[A19].right[A18].right[A17].right[A16].right[A15]
      .right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja21: Inj[Cop, A21] =
    Inj.instance(_.left[A22].right[A20].right[A19].right[A18].right[A17].right[A16].right[A15]
      .right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  implicit val inja22: Inj[Cop, A22] =
    Inj.instance(_.right[A21].right[A20].right[A19].right[A18].right[A17].right[A16].right[A15]
      .right[A14].right[A13].right[A12]
      .right[A11].right[A10].right[A9].right[A8]
      .right[A7].right[A6].right[A5].right[A4].right[A3].right[A2].right[A1])
  val injEv = combine[Inj.Aux[Cop]#Out].choose

  implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, A1] = {
    val (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, A2] = {
    val (a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, A3] = {
    val (a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, _, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, A4] = {
    val (a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, _, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, A5] = {
    val (a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, _, a6, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, A6] = {
    val (a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, _, a7, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, A7] = {
    val (a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, _, a8, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, A8] = {
    val (a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, _, a9, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, A9] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, _, a10, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, A10] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a11, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, A11] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a12, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta12(implicit M: Monoid[Prod]): Inj[Prod, A12] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a13, a14,
      a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta13(implicit M: Monoid[Prod]): Inj[Prod, A13] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14,
			a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _, a14,
			a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta14(implicit M: Monoid[Prod]): Inj[Prod, A14] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _,
			a15, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _,
			a15, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta15(implicit M: Monoid[Prod]): Inj[Prod, A15] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _, a16, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta16(implicit M: Monoid[Prod]): Inj[Prod, A16] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, _, a17, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, _, a17, a18, a19, a20, a21, a22))
  }
  implicit def lifta17(implicit M: Monoid[Prod]): Inj[Prod, A17] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, _, a18, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, _, a18, a19, a20, a21, a22))
  }
  implicit def lifta18(implicit M: Monoid[Prod]): Inj[Prod, A18] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, _, a19, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, _, a19, a20, a21, a22))
  }
  implicit def lifta19(implicit M: Monoid[Prod]): Inj[Prod, A19] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, _, a20, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, _, a20, a21, a22))
  }
  implicit def lifta20(implicit M: Monoid[Prod]): Inj[Prod, A20] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, a19, _, a21, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, a19, _, a21, a22))
  }
  implicit def lifta21(implicit M: Monoid[Prod]): Inj[Prod, A21] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, a19, a20, _, a22) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, a19, a20, _, a22))
  }
  implicit def lifta22(implicit M: Monoid[Prod]): Inj[Prod, A22] = {
    val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, a19, a20, a21, _) = M.zero
    Inj.instance((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
			a15, a16, a17, a18, a19, a20, a21, _))
  }
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide
}
