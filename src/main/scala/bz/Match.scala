package bz

import scalaz.{-\/, \/-}

sealed trait Match2[T1, T2]
case class M1[T1, T2](run: T1) extends Match2[T1, T2]
case class M2[T1, T2](run: T2) extends Match2[T1, T2]

object Match2 {

  implicit def Inj2[T1, T2]: Inj[Match2[T1, T2], TC2[T1, T2]#Cop] =
    new Inj[Match2[T1, T2], TC2[T1, T2]#Cop] {
      def apply(i: TC2[T1, T2]#Cop) = i match {
        case -\/(x) => M1(x)
        case \/-(x) => M2(x)
      }
  }

  def apply[T1, T2](i: TC2[T1, T2]#Cop): Match2[T1, T2] = Inj2(i)
}
