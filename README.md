Inspired by scalaz-deriving.

Provides macroless composition of covariant or contravariant
typeclasses for product and coproduct types.

Given a set of types, produces a representation of Coproduct as nested disjunctions,
as well as Product as a tuple.

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
SIS: bz.TC3[String,Int,List[String]] = $anon$1@5aadf421

scala> implicit val ds: Decidable[Show] = new Decidable[Show] { def choose2[Z, A1, A2](a1: => Show[A1], a2: =>Show[A2])(f: Z => (A1 \/ A2)): Show[Z] = Show.show[Z]((z: Z) => f(z).fold(a1.show(_), a2.show(_))) }
ds: bz.Decidable[scalaz.Show] = $anon$1@54f49fff

scala> SIS.combine[Show].choose.show("foo".left[Int \/ List[String]])
res0: scalaz.Cord = "foo"

scala> SIS.combine[Show].choose.show(2.left[List[String]].right[String])
res1: scalaz.Cord = 2

scala> SIS.combine[Show].choose.show(List("bar", "baz").right[Int].right[String])
res2: scalaz.Cord = ["bar","baz"]
```
