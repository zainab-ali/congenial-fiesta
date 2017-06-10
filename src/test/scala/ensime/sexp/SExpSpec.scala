package ensime
package sexp

import org.scalatest._

import cats.implicits._

class SExpSpec extends FlatSpec with SExpSyntax with Matchers {
  "Show[SExp]" should "show nil" in {
    elisp"nil".show shouldBe "nil"
  }

  it should "show a char" in {
    elisp"?c".show shouldBe "?c"
  }

  it should "show a list" in {
    elisp"(?c nil)".show shouldBe "(?c nil)"
  }

  it should "show a nested list" in {
    elisp"(?a (t ?b))".show shouldBe "(?a (t ?b))"
  }
}
