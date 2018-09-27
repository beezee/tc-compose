Inspired by scalaz-deriving.

Provides macroless composition of covariant or contravariant
typeclasses for product and coproduct types.

Given a set of types, produces a representation of Coproduct as nested disjunctions,
as well as Product as a tuple. Also provides an `inj` method for lifting a compatible type
into a Coproduct representation, and a `lift` method for lifting a compatible type
into a Product representation when that Product is monoidal.

With an instance of Decidable, Alt, Divide, or Apply for a given typeclass,
provides an instance for the corresponding Coproduct, or Product respectively.

```scala
scala> import bz.TC
import bz.TC

scala> import bz.Decidable
import bz.Decidable

scala> import scalaz.std.list._
import scalaz.std.list._

scala> import scalaz.std.anyVal._
import scalaz.std.anyVal._

scala> import scalaz.std.string._
import scalaz.std.string._

scala> import scalaz.std.tuple._
import scalaz.std.tuple._

scala> import scalaz.syntax.monoid._
import scalaz.syntax.monoid._

scala> import scalaz.syntax.either._
import scalaz.syntax.either._

scala> import scalaz.{Show, \/}
import scalaz.{Show, $bslash$div}

scala> val SIS = TC.combine[String, Int, List[String]]
SIS: bz.TC3[String,Int,List[String]] = bz.TC$$anon$6@6b7f046f

scala> import SIS._ // for inject instances
import SIS._

scala> implicit val ds: Decidable[Show] = new Decidable[Show] { def choose2[Z, A1, A2](a1: => Show[A1], a2: =>Show[A2])(f: Z => (A1 \/ A2)): Show[Z] = Show.show[Z]((z: Z) => f(z).fold(a1.show(_), a2.show(_))) }
ds: bz.Decidable[scalaz.Show] = $anon$1@11f9bfbe

scala> SIS.combine[Show].choose.show(SIS.inj("foo"))
res0: scalaz.Cord = "foo"

scala> SIS.combine[Show].choose.show(SIS.inj(2))
res1: scalaz.Cord = 2

scala> SIS.combine[Show].choose.show(SIS.inj(List("bar", "baz")))
res2: scalaz.Cord = ["bar","baz"]

scala> // lift into monoidal product
     | val SISL = TC.combineK[List, String, Int, List[String]]
SISL: bz.TC3[List[String],List[Int],List[List[String]]] = bz.TC$$anon$7@220d54e4

scala> import SISL._ // for inject instances
import SISL._

scala> SISL.lift(List(4)) |+| SISL.lift(List("foo")) |+| SISL.lift(List(List("bar")))
res4: SISL.Prod = (List(foo),List(4),List(List(bar)))
```
