package ensime
package sexp

import org.scalacheck._
import org.scalacheck.Arbitrary._

import matryoshka._
import matryoshka.data._


object lispArbitrary {

  //TODO: generate a list
  implicit def sexpArbitrary: Arbitrary[Fix[SExp]] = Arbitrary(
    Gen.oneOf(
      arbitrary[Int].map(i => Fix[SExp](SExp.Int(i))),
      arbitrary[Float].map(f => Fix[SExp](SExp.Float(f))),
      arbitrary[Char].map(c => Fix[SExp](SExp.Char(c))),
      arbitrary[String].map(s => Fix[SExp](SExp.Keyword(s))),
      arbitrary[String].map(s => Fix[SExp](SExp.Symbol(s))),
      Gen.oneOf(Fix[SExp](SExp.Nil), Fix[SExp](SExp.True)),
      sexpArbitrary.arbitrary.flatMap(h => sexpArbitrary.arbitrary.map(t => Fix[SExp](SExp.Pair(h, t))))
    )
  )

}
