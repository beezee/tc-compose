package bz

import iotaz.{Cop, Prod, TNil}
import iotaz.TList.::
import scala.annotation.switch
import scalaz.{\/, -\/, \/-}

trait TAux[A, B] {
  type In = A
  type Out = B
}
object TAux {
  def apply[A, B](fn: A => B): TAux[A, B] = new TAux[A, B] {}
}

object iotaSyntax {

  implicit class InjectOps[A](a: A) {
    def inj[B <: Cop[_]](implicit inj: Cop.Inject[A, B]): B = inj.inj(a)
  }
}

// this is in scalaz-deriving under IotaHelpers, but only up to arity of 4.
// not so useful to us and we don't need any of the rest of the library at present
object Cops {

  def to1[A1](c: Cop[A1 :: TNil]): A1 = c.value.asInstanceOf[A1]
  def to2[A1, A2](c: Cop[A1 :: A2 :: TNil]): A1 \/ A2 =
    (c.index: @switch) match {
      case 0 => -\/(c.value.asInstanceOf[A1])
      case 1 => \/-(c.value.asInstanceOf[A2])
    }
  def to3[A1, A2, A3](c: Cop[A1 :: A2 :: A3 :: TNil]): A1 \/ (A2 \/ A3) =
    (c.index: @switch) match {
      case 0 => -\/(c.value.asInstanceOf[A1])
      case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
      case 2 => \/-(\/-(c.value.asInstanceOf[A3]))
    }
  def to4[A1, A2, A3, A4](
    c: Cop[A1 :: A2 :: A3 :: A4 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ A4)) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(c.value.asInstanceOf[A4])))
  }
  def to5[A1, A2, A3, A4, A5](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ A5))) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
    case 4 => \/-(\/-(\/-(\/-(c.value.asInstanceOf[A5]))))
  }
  def to6[A1, A2, A3, A4, A5, A6](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6)))) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
    case 4 => \/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A5])))))
    case 5 => \/-(\/-(\/-(\/-(\/-(c.value.asInstanceOf[A6])))))
  }
  def to7[A1, A2, A3, A4, A5, A6, A7](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7))))) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
    case 4 => \/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A5])))))
    case 5 => \/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A6]))))))
    case 6 => \/-(\/-(\/-(\/-(\/-(\/-(c.value.asInstanceOf[A7]))))))
  }
  def to8[A1, A2, A3, A4, A5, A6, A7, A8](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8)))))) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
    case 4 => \/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A5])))))
    case 5 => \/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A6]))))))
    case 6 => \/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A7])))))))
    case 7 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(c.value.asInstanceOf[A8])))))))
  }
  def to9[A1, A2, A3, A4, A5, A6, A7, A8, A9](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9))))))) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
    case 4 => \/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A5])))))
    case 5 => \/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A6]))))))
    case 6 => \/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A7])))))))
    case 7 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A8]))))))))
    case 8 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(c.value.asInstanceOf[A9]))))))))
  }
  def to10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
    case 4 => \/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A5])))))
    case 5 => \/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A6]))))))
    case 6 => \/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A7])))))))
    case 7 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A8]))))))))
    case 8 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A9])))))))))
    case 9 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(c.value.asInstanceOf[A10])))))))))
  }
  def to11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: A11 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))) =
    (c.index: @switch) match {
      case 0 => -\/(c.value.asInstanceOf[A1])
      case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
      case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
      case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
      case 4 => \/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A5])))))
      case 5 => \/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A6]))))))
      case 6 => \/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A7])))))))
      case 7 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A8]))))))))
      case 8 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A9])))))))))
      case 9 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A10]))))))))))
      case 10 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(c.value.asInstanceOf[A11]))))))))))
    }
  def to12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: A11 :: A12 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))) =
    (c.index: @switch) match {
      case 0 => -\/(c.value.asInstanceOf[A1])
      case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
      case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
      case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
      case 4 => \/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A5])))))
      case 5 => \/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A6]))))))
      case 6 => \/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A7])))))))
      case 7 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A8]))))))))
      case 8 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A9])))))))))
      case 9 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A10]))))))))))
      case 10 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A11])))))))))))
      case 11 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(c.value.asInstanceOf[A12])))))))))))
    }
  def to13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
    case 4 => \/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A5])))))
    case 5 => \/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A6]))))))
    case 6 => \/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A7])))))))
    case 7 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A8]))))))))
    case 8 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A9])))))))))
    case 9 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A10]))))))))))
    case 10 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A11])))))))))))
    case 11 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A12]))))))))))))
    case 12 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(c.value.asInstanceOf[A13]))))))))))))
  }
  def to14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
           A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
     (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
    case 4 => \/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A5])))))
    case 5 => \/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A6]))))))
    case 6 => \/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A7])))))))
    case 7 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A8]))))))))
    case 8 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A9])))))))))
    case 9 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A10]))))))))))
    case 10 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A11])))))))))))
    case 11 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A12]))))))))))))
    case 12 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A13])))))))))))))
    case 13 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(c.value.asInstanceOf[A14])))))))))))))
  }
  def to15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
           A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 ::
           A15 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
     (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
     A15))))))))))))) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
    case 4 => \/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A5])))))
    case 5 => \/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A6]))))))
    case 6 => \/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A7])))))))
    case 7 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A8]))))))))
    case 8 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A9])))))))))
    case 9 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A10]))))))))))
    case 10 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A11])))))))))))
    case 11 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A12]))))))))))))
    case 12 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A13])))))))))))))
    case 13 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(
      -\/(c.value.asInstanceOf[A14]))))))))))))))
    case 14 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(
      \/-(c.value.asInstanceOf[A15]))))))))))))))
  }
  def to16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
           A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 ::
           A15 :: A16 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
     (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
     (A15 \/ A16)))))))))))))) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
    case 4 => \/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A5])))))
    case 5 => \/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A6]))))))
    case 6 => \/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A7])))))))
    case 7 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A8]))))))))
    case 8 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A9])))))))))
    case 9 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A10]))))))))))
    case 10 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A11])))))))))))
    case 11 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A12]))))))))))))
    case 12 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A13])))))))))))))
    case 13 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(
      -\/(c.value.asInstanceOf[A14]))))))))))))))
    case 14 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(
      \/-(-\/(c.value.asInstanceOf[A15])))))))))))))))
    case 15 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(
      \/-(\/-(c.value.asInstanceOf[A16])))))))))))))))
  }
  def to17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
           A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 ::
           A15 :: A16 :: A17 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
     (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
     (A15 \/ (A16 \/ A17))))))))))))))) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
    case 4 => \/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A5])))))
    case 5 => \/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A6]))))))
    case 6 => \/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A7])))))))
    case 7 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A8]))))))))
    case 8 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A9])))))))))
    case 9 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A10]))))))))))
    case 10 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A11])))))))))))
    case 11 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A12]))))))))))))
    case 12 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(c.value.asInstanceOf[A13])))))))))))))
    case 13 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(
      -\/(c.value.asInstanceOf[A14]))))))))))))))
    case 14 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(
      \/-(-\/(c.value.asInstanceOf[A15])))))))))))))))
    case 15 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(
      \/-(\/-(-\/(c.value.asInstanceOf[A16]))))))))))))))))
    case 16 => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(
      \/-(\/-(\/-(c.value.asInstanceOf[A17]))))))))))))))))
  }

   def from1[A1](e: A1): Cop[A1 :: TNil] = Cop.unsafeApply(0, e)
   def from2[A1, A2](e: A1 \/ A2): Cop[A1 :: A2 :: TNil] = e match {
     case -\/(a) => Cop.unsafeApply(0, a)
     case \/-(b) => Cop.unsafeApply(1, b)
   }
   def from3[A1, A2, A3](e: A1 \/ (A2 \/ A3)): Cop[A1 :: A2 :: A3 :: TNil] =
     e match {
       case -\/(a)      => Cop.unsafeApply(0, a)
       case \/-(-\/(b)) => Cop.unsafeApply(1, b)
       case \/-(\/-(c)) => Cop.unsafeApply(2, c)
     }
   def from4[A1, A2, A3, A4](
     e: A1 \/ (A2 \/ (A3 \/ A4))
   ): Cop[A1 :: A2 :: A3 :: A4 :: TNil] =
     e match {
       case -\/(a)           => Cop.unsafeApply(0, a)
       case \/-(-\/(b))      => Cop.unsafeApply(1, b)
       case \/-(\/-(-\/(c))) => Cop.unsafeApply(2, c)
       case \/-(\/-(\/-(d))) => Cop.unsafeApply(3, d)
     }
   def from5[A1, A2, A3, A4, A5](
     e: A1 \/ (A2 \/ (A3 \/ (A4 \/ A5)))
   ): Cop[A1 :: A2 :: A3 :: A4 :: A5 :: TNil] =
     e match {
       case -\/(a)           => Cop.unsafeApply(0, a)
       case \/-(-\/(b))      => Cop.unsafeApply(1, b)
       case \/-(\/-(-\/(c))) => Cop.unsafeApply(2, c)
       case \/-(\/-(\/-(-\/(d)))) => Cop.unsafeApply(3, d)
       case \/-(\/-(\/-(\/-(e)))) => Cop.unsafeApply(4, e)
     }
   def from6[A1, A2, A3, A4, A5, A6](
     e: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6))))
   ): Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: TNil] =
     e match {
       case -\/(a1)           => Cop.unsafeApply(0, a1)
       case \/-(-\/(a2))      => Cop.unsafeApply(1, a2)
       case \/-(\/-(-\/(a3))) => Cop.unsafeApply(2, a3)
       case \/-(\/-(\/-(-\/(a4)))) => Cop.unsafeApply(3, a4)
       case \/-(\/-(\/-(\/-(-\/(a5))))) => Cop.unsafeApply(4, a5)
       case \/-(\/-(\/-(\/-(\/-(a6))))) => Cop.unsafeApply(5, a6)
     }
   def from7[A1, A2, A3, A4, A5, A6, A7](
     e: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))))
   ): Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: TNil] =
     e match {
       case -\/(a1)           => Cop.unsafeApply(0, a1)
       case \/-(-\/(a2))      => Cop.unsafeApply(1, a2)
       case \/-(\/-(-\/(a3))) => Cop.unsafeApply(2, a3)
       case \/-(\/-(\/-(-\/(a4)))) => Cop.unsafeApply(3, a4)
       case \/-(\/-(\/-(\/-(-\/(a5))))) => Cop.unsafeApply(4, a5)
       case \/-(\/-(\/-(\/-(\/-(-\/(a6)))))) => Cop.unsafeApply(5, a6)
       case \/-(\/-(\/-(\/-(\/-(\/-(a7)))))) => Cop.unsafeApply(6, a7)
     }
   def from8[A1, A2, A3, A4, A5, A6, A7, A8](
     e: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))
   ): Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: TNil] =
     e match {
       case -\/(a1)           => Cop.unsafeApply(0, a1)
       case \/-(-\/(a2))      => Cop.unsafeApply(1, a2)
       case \/-(\/-(-\/(a3))) => Cop.unsafeApply(2, a3)
       case \/-(\/-(\/-(-\/(a4)))) => Cop.unsafeApply(3, a4)
       case \/-(\/-(\/-(\/-(-\/(a5))))) => Cop.unsafeApply(4, a5)
       case \/-(\/-(\/-(\/-(\/-(-\/(a6)))))) => Cop.unsafeApply(5, a6)
       case \/-(\/-(\/-(\/-(\/-(\/-(-\/(a7))))))) => Cop.unsafeApply(6, a7)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(a8))))))) => Cop.unsafeApply(7, a8)
     }
   def from9[A1, A2, A3, A4, A5, A6, A7, A8, A9](
     e: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))
   ): Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: TNil] =
     e match {
       case -\/(a1)           => Cop.unsafeApply(0, a1)
       case \/-(-\/(a2))      => Cop.unsafeApply(1, a2)
       case \/-(\/-(-\/(a3))) => Cop.unsafeApply(2, a3)
       case \/-(\/-(\/-(-\/(a4)))) => Cop.unsafeApply(3, a4)
       case \/-(\/-(\/-(\/-(-\/(a5))))) => Cop.unsafeApply(4, a5)
       case \/-(\/-(\/-(\/-(\/-(-\/(a6)))))) => Cop.unsafeApply(5, a6)
       case \/-(\/-(\/-(\/-(\/-(\/-(-\/(a7))))))) => Cop.unsafeApply(6, a7)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a8)))))))) => Cop.unsafeApply(7, a8)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(a9)))))))) => Cop.unsafeApply(8, a9)
     }
   def from10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
     e: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))
   ): Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 ::TNil] =
     e match {
       case -\/(a1)           => Cop.unsafeApply(0, a1)
       case \/-(-\/(a2))      => Cop.unsafeApply(1, a2)
       case \/-(\/-(-\/(a3))) => Cop.unsafeApply(2, a3)
       case \/-(\/-(\/-(-\/(a4)))) => Cop.unsafeApply(3, a4)
       case \/-(\/-(\/-(\/-(-\/(a5))))) => Cop.unsafeApply(4, a5)
       case \/-(\/-(\/-(\/-(\/-(-\/(a6)))))) => Cop.unsafeApply(5, a6)
       case \/-(\/-(\/-(\/-(\/-(\/-(-\/(a7))))))) => Cop.unsafeApply(6, a7)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a8)))))))) => Cop.unsafeApply(7, a8)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a9))))))))) => Cop.unsafeApply(8, a9)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(a10))))))))) => Cop.unsafeApply(9, a10)
     }
   def from11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
     e: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))
   ): Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: A11 :: TNil] =
     e match {
       case -\/(a1)           => Cop.unsafeApply(0, a1)
       case \/-(-\/(a2))      => Cop.unsafeApply(1, a2)
       case \/-(\/-(-\/(a3))) => Cop.unsafeApply(2, a3)
       case \/-(\/-(\/-(-\/(a4)))) => Cop.unsafeApply(3, a4)
       case \/-(\/-(\/-(\/-(-\/(a5))))) => Cop.unsafeApply(4, a5)
       case \/-(\/-(\/-(\/-(\/-(-\/(a6)))))) => Cop.unsafeApply(5, a6)
       case \/-(\/-(\/-(\/-(\/-(\/-(-\/(a7))))))) => Cop.unsafeApply(6, a7)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a8)))))))) => Cop.unsafeApply(7, a8)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a9))))))))) => Cop.unsafeApply(8, a9)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a10)))))))))) => Cop.unsafeApply(9, a10)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(a11)))))))))) => Cop.unsafeApply(10, a11)
     }
   def from12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
     e: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))
   ): Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: A11 :: A12 :: TNil] =
     e match {
       case -\/(a1)           => Cop.unsafeApply(0, a1)
       case \/-(-\/(a2))      => Cop.unsafeApply(1, a2)
       case \/-(\/-(-\/(a3))) => Cop.unsafeApply(2, a3)
       case \/-(\/-(\/-(-\/(a4)))) => Cop.unsafeApply(3, a4)
       case \/-(\/-(\/-(\/-(-\/(a5))))) => Cop.unsafeApply(4, a5)
       case \/-(\/-(\/-(\/-(\/-(-\/(a6)))))) => Cop.unsafeApply(5, a6)
       case \/-(\/-(\/-(\/-(\/-(\/-(-\/(a7))))))) => Cop.unsafeApply(6, a7)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a8)))))))) => Cop.unsafeApply(7, a8)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a9))))))))) => Cop.unsafeApply(8, a9)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a10)))))))))) => Cop.unsafeApply(9, a10)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a11))))))))))) => Cop.unsafeApply(10, a11)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(a12))))))))))) => Cop.unsafeApply(11, a12)
     }
   def from13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
     e: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))
   ): Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: TNil] =
     e match {
       case -\/(a1)           => Cop.unsafeApply(0, a1)
       case \/-(-\/(a2))      => Cop.unsafeApply(1, a2)
       case \/-(\/-(-\/(a3))) => Cop.unsafeApply(2, a3)
       case \/-(\/-(\/-(-\/(a4)))) => Cop.unsafeApply(3, a4)
       case \/-(\/-(\/-(\/-(-\/(a5))))) => Cop.unsafeApply(4, a5)
       case \/-(\/-(\/-(\/-(\/-(-\/(a6)))))) => Cop.unsafeApply(5, a6)
       case \/-(\/-(\/-(\/-(\/-(\/-(-\/(a7))))))) => Cop.unsafeApply(6, a7)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a8)))))))) => Cop.unsafeApply(7, a8)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a9))))))))) => Cop.unsafeApply(8, a9)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a10)))))))))) => Cop.unsafeApply(9, a10)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a11))))))))))) => Cop.unsafeApply(10, a11)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a12)))))))))))) => Cop.unsafeApply(11, a12)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(a13)))))))))))) => Cop.unsafeApply(12, a13)
     }
   def from14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
     e: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
        (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))
   ): Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
          A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 :: TNil] =
     e match {
       case -\/(a1)           => Cop.unsafeApply(0, a1)
       case \/-(-\/(a2))      => Cop.unsafeApply(1, a2)
       case \/-(\/-(-\/(a3))) => Cop.unsafeApply(2, a3)
       case \/-(\/-(\/-(-\/(a4)))) => Cop.unsafeApply(3, a4)
       case \/-(\/-(\/-(\/-(-\/(a5))))) => Cop.unsafeApply(4, a5)
       case \/-(\/-(\/-(\/-(\/-(-\/(a6)))))) => Cop.unsafeApply(5, a6)
       case \/-(\/-(\/-(\/-(\/-(\/-(-\/(a7))))))) => Cop.unsafeApply(6, a7)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a8)))))))) => Cop.unsafeApply(7, a8)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a9))))))))) => Cop.unsafeApply(8, a9)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a10)))))))))) => Cop.unsafeApply(9, a10)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a11))))))))))) => Cop.unsafeApply(10, a11)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a12)))))))))))) => Cop.unsafeApply(11, a12)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a13))))))))))))) =>
        Cop.unsafeApply(12, a13)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(a14))))))))))))) =>
        Cop.unsafeApply(13, a14)
     }
   def from15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
     e: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
        (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
        A15)))))))))))))
   ): Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
          A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 :: A15 :: TNil] =
     e match {
       case -\/(a1)           => Cop.unsafeApply(0, a1)
       case \/-(-\/(a2))      => Cop.unsafeApply(1, a2)
       case \/-(\/-(-\/(a3))) => Cop.unsafeApply(2, a3)
       case \/-(\/-(\/-(-\/(a4)))) => Cop.unsafeApply(3, a4)
       case \/-(\/-(\/-(\/-(-\/(a5))))) => Cop.unsafeApply(4, a5)
       case \/-(\/-(\/-(\/-(\/-(-\/(a6)))))) => Cop.unsafeApply(5, a6)
       case \/-(\/-(\/-(\/-(\/-(\/-(-\/(a7))))))) => Cop.unsafeApply(6, a7)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a8)))))))) => Cop.unsafeApply(7, a8)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a9))))))))) => Cop.unsafeApply(8, a9)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a10)))))))))) => Cop.unsafeApply(9, a10)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a11))))))))))) => Cop.unsafeApply(10, a11)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a12)))))))))))) => Cop.unsafeApply(11, a12)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a13))))))))))))) =>
        Cop.unsafeApply(12, a13)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a14)))))))))))))) =>
        Cop.unsafeApply(13, a14)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(a15)))))))))))))) =>
        Cop.unsafeApply(14, a15)
     }
   def from16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
     e: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
        (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
        (A15 \/ A16))))))))))))))
   ): Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
          A8 :: A9 :: A10 :: A11 :: A12 :: A13 ::
          A14 :: A15 :: A16 :: TNil] =
     e match {
       case -\/(a1)           => Cop.unsafeApply(0, a1)
       case \/-(-\/(a2))      => Cop.unsafeApply(1, a2)
       case \/-(\/-(-\/(a3))) => Cop.unsafeApply(2, a3)
       case \/-(\/-(\/-(-\/(a4)))) => Cop.unsafeApply(3, a4)
       case \/-(\/-(\/-(\/-(-\/(a5))))) => Cop.unsafeApply(4, a5)
       case \/-(\/-(\/-(\/-(\/-(-\/(a6)))))) => Cop.unsafeApply(5, a6)
       case \/-(\/-(\/-(\/-(\/-(\/-(-\/(a7))))))) => Cop.unsafeApply(6, a7)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a8)))))))) => Cop.unsafeApply(7, a8)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a9))))))))) => Cop.unsafeApply(8, a9)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a10)))))))))) => Cop.unsafeApply(9, a10)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a11))))))))))) => Cop.unsafeApply(10, a11)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a12)))))))))))) => Cop.unsafeApply(11, a12)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a13))))))))))))) =>
        Cop.unsafeApply(12, a13)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a14)))))))))))))) =>
        Cop.unsafeApply(13, a14)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a15))))))))))))))) =>
        Cop.unsafeApply(14, a15)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(a16))))))))))))))) =>
        Cop.unsafeApply(15, a16)
     }
   def from17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
     e: A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/
        (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/
        (A15 \/ (A16 \/ A17)))))))))))))))
   ): Cop[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
          A8 :: A9 :: A10 :: A11 :: A12 :: A13 ::
          A14 :: A15 :: A16 :: A17 :: TNil] =
     e match {
       case -\/(a1)           => Cop.unsafeApply(0, a1)
       case \/-(-\/(a2))      => Cop.unsafeApply(1, a2)
       case \/-(\/-(-\/(a3))) => Cop.unsafeApply(2, a3)
       case \/-(\/-(\/-(-\/(a4)))) => Cop.unsafeApply(3, a4)
       case \/-(\/-(\/-(\/-(-\/(a5))))) => Cop.unsafeApply(4, a5)
       case \/-(\/-(\/-(\/-(\/-(-\/(a6)))))) => Cop.unsafeApply(5, a6)
       case \/-(\/-(\/-(\/-(\/-(\/-(-\/(a7))))))) => Cop.unsafeApply(6, a7)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a8)))))))) => Cop.unsafeApply(7, a8)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a9))))))))) => Cop.unsafeApply(8, a9)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a10)))))))))) => Cop.unsafeApply(9, a10)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a11))))))))))) => Cop.unsafeApply(10, a11)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a12)))))))))))) => Cop.unsafeApply(11, a12)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a13))))))))))))) =>
        Cop.unsafeApply(12, a13)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a14)))))))))))))) =>
        Cop.unsafeApply(13, a14)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(a15))))))))))))))) =>
        Cop.unsafeApply(14, a15)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(
          -\/(a16)))))))))))))))) =>
        Cop.unsafeApply(15, a16)
       case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(
          \/-(a17)))))))))))))))) =>
        Cop.unsafeApply(16, a17)
     }
}

object Prods {
  import scala.Array
  import iotaz.internal.ArraySeq

  val empty: Prod[TNil] = Prod()
  def from1T[A1](e: A1): Prod[A1 :: TNil] =
    Prod.unsafeApply(new ArraySeq(Array(e)))
  def from2T[A1, A2](e: (A1, A2)): Prod[A1 :: A2 :: TNil] =
    Prod.unsafeApply(new ArraySeq(Array(e._1, e._2)))
  def from3T[A1, A2, A3](e: (A1, A2, A3)): Prod[A1 :: A2 :: A3 :: TNil] =
    Prod.unsafeApply(new ArraySeq(Array(e._1, e._2, e._3)))
  def from4T[A1, A2, A3, A4](
    e: (A1, A2, A3, A4)
  ): Prod[A1 :: A2 :: A3 :: A4 :: TNil] =
    Prod.unsafeApply(new ArraySeq(Array(e._1, e._2, e._3, e._4)))
  def from5T[A1, A2, A3, A4, A5](
    e: (A1, A2, A3, A4, A5)
  ): Prod[A1 :: A2 :: A3 :: A4 :: A5 :: TNil] =
    Prod.unsafeApply(new ArraySeq(Array(e._1, e._2, e._3, e._4, e._5)))
  def from6T[A1, A2, A3, A4, A5, A6](
    e: (A1, A2, A3, A4, A5, A6)
  ): Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: TNil] =
    Prod.unsafeApply(new ArraySeq(Array(e._1, e._2, e._3, e._4, e._5, e._6)))
  def from7T[A1, A2, A3, A4, A5, A6, A7](
    e: (A1, A2, A3, A4, A5, A6, A7)
  ): Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: TNil] =
    Prod.unsafeApply(new ArraySeq(Array(e._1, e._2, e._3, e._4, e._5, e._6, e._7)))
  def from8T[A1, A2, A3, A4, A5, A6, A7, A8](
    e: (A1, A2, A3, A4, A5, A6, A7, A8)
  ): Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: TNil] =
    Prod.unsafeApply(new ArraySeq(Array(e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8)))
  def from9T[A1, A2, A3, A4, A5, A6, A7, A8, A9](
    e: (A1, A2, A3, A4, A5, A6, A7, A8, A9)
  ): Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: TNil] =
    Prod.unsafeApply(new ArraySeq(Array(e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8, e._9)))
  def from10T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    e: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)
  ): Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: TNil] =
    Prod.unsafeApply(new ArraySeq(Array(e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8, e._9, e._10)))
  def from11T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    e: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)
  ): Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: A11 :: TNil] =
    Prod.unsafeApply(
      new ArraySeq(
        Array(e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8, e._9, e._10, e._11)))
  def from12T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    e: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
  ): Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: A11 :: A12 :: TNil] =
    Prod.unsafeApply(
      new ArraySeq(
        Array(e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8, e._9, e._10, e._11, e._12)))
  def from13T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    e: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)
  ): Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: TNil] =
    Prod.unsafeApply(
      new ArraySeq(
        Array(e._1, e._2, e._3, e._4, e._5, e._6, e._7, e._8, e._9, e._10, e._11, e._12, e._13)))
  def from14T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    e: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)
  ): Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
          A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 :: TNil] =
    Prod.unsafeApply(
      new ArraySeq(
        Array(e._1, e._2, e._3, e._4, e._5, e._6, e._7,
              e._8, e._9, e._10, e._11, e._12, e._13, e._14)))
  def from15T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
    e: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)
  ): Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
          A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 :: A15 :: TNil] =
    Prod.unsafeApply(
      new ArraySeq(
        Array(e._1, e._2, e._3, e._4, e._5, e._6, e._7,
              e._8, e._9, e._10, e._11, e._12, e._13, e._14,
              e._15)))
  def from16T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
    e: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
  ): Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
          A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 :: A15 :: A16 :: TNil] =
    Prod.unsafeApply(
      new ArraySeq(
        Array(e._1, e._2, e._3, e._4, e._5, e._6, e._7,
              e._8, e._9, e._10, e._11, e._12, e._13, e._14,
              e._15, e._16)))
  def from17T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
    e: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)
  ): Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
          A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 :: A15 :: A16 ::
          A17 :: TNil] =
    Prod.unsafeApply(
      new ArraySeq(
        Array(e._1, e._2, e._3, e._4, e._5, e._6, e._7,
              e._8, e._9, e._10, e._11, e._12, e._13, e._14,
              e._15, e._16, e._17)))

  def to1T[A1](a: Prod[A1 :: TNil]): A1 = a.values(0).asInstanceOf[A1]
  def to2T[A1, A2](a: Prod[A1 :: A2 :: TNil]): (A1, A2) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2]
  )
  def to3T[A1, A2, A3](a: Prod[A1 :: A2 :: A3 :: TNil]): (A1, A2, A3) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3]
  )
  def to4T[A1, A2, A3, A4](
    a: Prod[A1 :: A2 :: A3 :: A4 :: TNil]
  ): (A1, A2, A3, A4) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4])
  def to5T[A1, A2, A3, A4, A5](
    a: Prod[A1 :: A2 :: A3 :: A4 :: A5 :: TNil]
  ): (A1, A2, A3, A4, A5) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4],
    a.values(4).asInstanceOf[A5])
  def to6T[A1, A2, A3, A4, A5, A6](
    a: Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: TNil]
  ): (A1, A2, A3, A4, A5, A6) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4],
    a.values(4).asInstanceOf[A5],
    a.values(5).asInstanceOf[A6])
  def to7T[A1, A2, A3, A4, A5, A6, A7](
    a: Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: TNil]
  ): (A1, A2, A3, A4, A5, A6, A7) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4],
    a.values(4).asInstanceOf[A5],
    a.values(5).asInstanceOf[A6],
    a.values(6).asInstanceOf[A7])
  def to8T[A1, A2, A3, A4, A5, A6, A7, A8](
    a: Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: TNil]
  ): (A1, A2, A3, A4, A5, A6, A7, A8) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4],
    a.values(4).asInstanceOf[A5],
    a.values(5).asInstanceOf[A6],
    a.values(6).asInstanceOf[A7],
    a.values(7).asInstanceOf[A8])
  def to9T[A1, A2, A3, A4, A5, A6, A7, A8, A9](
    a: Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: TNil]
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4],
    a.values(4).asInstanceOf[A5],
    a.values(5).asInstanceOf[A6],
    a.values(6).asInstanceOf[A7],
    a.values(7).asInstanceOf[A8],
    a.values(8).asInstanceOf[A9])
  def to10T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    a: Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 ::TNil]
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4],
    a.values(4).asInstanceOf[A5],
    a.values(5).asInstanceOf[A6],
    a.values(6).asInstanceOf[A7],
    a.values(7).asInstanceOf[A8],
    a.values(8).asInstanceOf[A9],
    a.values(9).asInstanceOf[A10])
  def to11T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    a: Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: A11 :: TNil]
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4],
    a.values(4).asInstanceOf[A5],
    a.values(5).asInstanceOf[A6],
    a.values(6).asInstanceOf[A7],
    a.values(7).asInstanceOf[A8],
    a.values(8).asInstanceOf[A9],
    a.values(9).asInstanceOf[A10],
    a.values(10).asInstanceOf[A11])
  def to12T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    a: Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: A11 :: A12 :: TNil]
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4],
    a.values(4).asInstanceOf[A5],
    a.values(5).asInstanceOf[A6],
    a.values(6).asInstanceOf[A7],
    a.values(7).asInstanceOf[A8],
    a.values(8).asInstanceOf[A9],
    a.values(9).asInstanceOf[A10],
    a.values(10).asInstanceOf[A11],
    a.values(11).asInstanceOf[A12])
  def to13T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    a: Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: TNil]
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4],
    a.values(4).asInstanceOf[A5],
    a.values(5).asInstanceOf[A6],
    a.values(6).asInstanceOf[A7],
    a.values(7).asInstanceOf[A8],
    a.values(8).asInstanceOf[A9],
    a.values(9).asInstanceOf[A10],
    a.values(10).asInstanceOf[A11],
    a.values(11).asInstanceOf[A12],
    a.values(12).asInstanceOf[A13])
  def to14T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    a: Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 :: TNil]
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4],
    a.values(4).asInstanceOf[A5],
    a.values(5).asInstanceOf[A6],
    a.values(6).asInstanceOf[A7],
    a.values(7).asInstanceOf[A8],
    a.values(8).asInstanceOf[A9],
    a.values(9).asInstanceOf[A10],
    a.values(10).asInstanceOf[A11],
    a.values(11).asInstanceOf[A12],
    a.values(12).asInstanceOf[A13],
    a.values(13).asInstanceOf[A14])
  def to15T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
    a: Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 :: A15 :: TNil]
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4],
    a.values(4).asInstanceOf[A5],
    a.values(5).asInstanceOf[A6],
    a.values(6).asInstanceOf[A7],
    a.values(7).asInstanceOf[A8],
    a.values(8).asInstanceOf[A9],
    a.values(9).asInstanceOf[A10],
    a.values(10).asInstanceOf[A11],
    a.values(11).asInstanceOf[A12],
    a.values(12).asInstanceOf[A13],
    a.values(13).asInstanceOf[A14],
    a.values(14).asInstanceOf[A15])
  def to16T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
    a: Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 ::
            A15 :: A16 :: TNil]
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4],
    a.values(4).asInstanceOf[A5],
    a.values(5).asInstanceOf[A6],
    a.values(6).asInstanceOf[A7],
    a.values(7).asInstanceOf[A8],
    a.values(8).asInstanceOf[A9],
    a.values(9).asInstanceOf[A10],
    a.values(10).asInstanceOf[A11],
    a.values(11).asInstanceOf[A12],
    a.values(12).asInstanceOf[A13],
    a.values(13).asInstanceOf[A14],
    a.values(14).asInstanceOf[A15],
    a.values(15).asInstanceOf[A16])
  def to17T[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
    a: Prod[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 ::
            A8 :: A9 :: A10 :: A11 :: A12 :: A13 :: A14 ::
            A15 :: A16 :: A17 :: TNil]
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4],
    a.values(4).asInstanceOf[A5],
    a.values(5).asInstanceOf[A6],
    a.values(6).asInstanceOf[A7],
    a.values(7).asInstanceOf[A8],
    a.values(8).asInstanceOf[A9],
    a.values(9).asInstanceOf[A10],
    a.values(10).asInstanceOf[A11],
    a.values(11).asInstanceOf[A12],
    a.values(12).asInstanceOf[A13],
    a.values(13).asInstanceOf[A14],
    a.values(14).asInstanceOf[A15],
    a.values(15).asInstanceOf[A16],
    a.values(16).asInstanceOf[A17])
}
