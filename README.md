Inspired by scalaz-deriving.

Provides macroless composition of covariant or contravariant
typeclasses for product and coproduct types.

Given a set of types, produces a representation of Coproduct as nested disjunctions,
as well as Product as a tuple. Also provides an `inj` method for lifting a compatible type
into a Coproduct representation.

With an instance of Decidable, Alt, Divide, or Apply for a given typeclass,
provides an instance for the corresponding Coproduct, or Product respectively.

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
SIS: bz.TC3[String,Int,List[String]] = $anon$1@5ee7c045

scala> import SIS._ // for inject instances
import SIS._

scala> implicit val ds: Decidable[Show] = new Decidable[Show] { def choose2[Z, A1, A2](a1: => Show[A1], a2: =>Show[A2])(f: Z => (A1 \/ A2)): Show[Z] = Show.show[Z]((z: Z) => f(z).fold(a1.show(_), a2.show(_))) }
ds: bz.Decidable[scalaz.Show] = $anon$1@2339edea

scala> SIS.combine[Show].choose.show(SIS.inj("foo"))
res0: scalaz.Cord = "foo"

scala> SIS.combine[Show].choose.show(SIS.inj(2))
res1: scalaz.Cord = 2

scala> SIS.combine[Show].choose.show(SIS.inj(List("bar", "baz")))
res2: scalaz.Cord = ["bar","baz"]
```
