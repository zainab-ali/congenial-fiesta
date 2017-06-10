package ensime
package sexp

import org.scalatest._

import cats.implicits._

class SExpSpec extends FlatSpec with SExpSyntax with Matchers {
  "Show[SExp]" should "show nil" in {
    elisp"nil".show shouldBe "nil"
  }

  it should "show chars" in {
    elisp"?c".show shouldBe "?c"
  }
}
