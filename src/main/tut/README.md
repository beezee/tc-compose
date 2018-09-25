Inspired by scalaz-deriving.

Provides macroless composition of covariant or contravariant
typeclasses for product and coproduct types.

Given a set of types, produces a representation of Coproduct as nested disjunctions,
as well as Product as a tuple. Also provides an `inj` method for lifting a compatible type
into a Coproduct representation.

With an instance of Decidable, Alt, Divide, or Apply for a given typeclass,
provides an instance for the corresponding Coproduct, or Product respectively.

```tut
import bz.TC3
import bz.Decidable
import scalaz.std.list._
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.syntax.either._
import scalaz.{Show, \/}
val SIS = new TC3[String, Int, List[String]] {}
import SIS._ // for inject instances
implicit val ds: Decidable[Show] = new Decidable[Show] { def choose2[Z, A1, A2](a1: => Show[A1], a2: =>Show[A2])(f: Z => (A1 \/ A2)): Show[Z] = Show.show[Z]((z: Z) => f(z).fold(a1.show(_), a2.show(_))) }
SIS.combine[Show].choose.show(SIS.inj("foo"))
SIS.combine[Show].choose.show(SIS.inj(2))
SIS.combine[Show].choose.show(SIS.inj(List("bar", "baz")))
```
