package ensime
package sexp

import org.scalatest._

import matryoshka.data._
import matryoshka.implicits._

final class ELispParserSpec extends FlatSpec with SExpSyntax with Matchers {

  val parser = ELispParser[Fix]

  import parser._

  "An elisp parser" should "be able to parse nil" in {
    parse("nil") shouldBe Right(Fix[SExp](SExp.Nil))
  }
}
