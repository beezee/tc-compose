Inspired by scalaz-deriving.

Provides macroless (save for iota) composition of covariant or contravariant
typeclasses for product and coproduct types.

Given an iotaz.TList, produces isomorphisms between Cop and a nested disjunction,
as well as Prod and a tuple.

With an instance of Decidable, Alt, Divide, or Apply for a given typeclass,
provides an instance for the corresponding Cop / nested disjunction, or Prod / tuple
respectively.

```scala
scala> import bz.TC3
import bz.TC3

scala> import bz.Decidable
import bz.Decidable

scala> import scalaz.std.list._
import scalaz.std.list._

scala> import scalaz.std.anyVal._
import scalaz.std.anyVal._

scala> import scalaz.std.string._
import scalaz.std.string._

scala> import scalaz.syntax.either._
import scalaz.syntax.either._

scala> import scalaz.{Show, \/}
import scalaz.{Show, $bslash$div}

scala> val SIS = new TC3[String, Int, List[String]] {}
SIS: bz.TC3[String,Int,List[String]] = $anon$1@1d19d701

scala> implicit val ds: Decidable[Show] = new Decidable[Show] { def choose2[Z, A1, A2](a1: => Show[A1], a2: =>Show[A2])(f: Z => (A1 \/ A2)): Show[Z] = Show.show[Z]((z: Z) => f(z).fold(a1.show(_), a2.show(_))) }
ds: bz.Decidable[scalaz.Show] = $anon$1@297f49ac

scala> SIS.combine[Show].chooseI.show(SIS.inj("foo"))
res0: scalaz.Cord = "foo"

scala> SIS.combine[Show].chooseI.show(SIS.inj(2))
res1: scalaz.Cord = 2

scala> SIS.combine[Show].chooseI.show(SIS.inj(List("bar", "baz")))
res2: scalaz.Cord = ["bar","baz"]

scala> SIS.combine[Show].choose.show("foo".left[Int \/ List[String]])
res3: scalaz.Cord = "foo"

scala> SIS.combine[Show].choose.show(2.left[List[String]].right[String])
res4: scalaz.Cord = 2

scala> SIS.combine[Show].choose.show(List("bar", "baz").right[Int].right[String])
res5: scalaz.Cord = ["bar","baz"]
```
