package ensime
package asm

import fastparse.all._


object SignatureParser {


  val packageName: Parser[PackageName] = P((((!CharIn("<;/ ")).rep(1).! ~ "/")rep).map(seq => PackageName(seq.toList)))
  def name: Parser[String] = P((!CharIn("<;/ ")).rep(1).!)
  val classNameSig: Parser[ClassName] = P("L" ~ packageName ~ name).map((ClassName.apply _).tupled)

  //duplicated
  val boolean: Parser[ClassName] = P("Z").map(_ => ClassName.primitiveBoolean)
  val byte: Parser[ClassName] = P("B").map(_ => ClassName.primitiveByte)
  val char: Parser[ClassName] = P("C").map(_ => ClassName.primitiveChar)
  val short: Parser[ClassName] = P("S").map(_ => ClassName.primitiveShort)
  val int: Parser[ClassName] = P("I").map(_ => ClassName.primitiveInt)
  val long: Parser[ClassName] = P("J").map(_ => ClassName.primitiveLong)
  val float: Parser[ClassName] = P("F").map(_ => ClassName.primitiveFloat)
  val double: Parser[ClassName] = P("D").map(_ => ClassName.primitiveDouble)
  val void: Parser[ClassName] = P("V").map(_ => ClassName.primitiveVoid)

  val primitiveType: Parser[ClassName] =
    P(boolean | byte | char | short | int | long | float | double | void)

  val lowerBoundary: Parser[BoundType] =
    P("-")
      .map(_ => LowerBound)

  val upperBoundary: Parser[BoundType] =
    P("+")
      .map(_ => UpperBound)


  val primitiveClassSig: Parser[GenericClassName] =
    P(primitiveType)
      .map(GenericClassName(_, Seq.empty))

  val genericName: Parser[String] = P((!CharIn(":;/ ")).rep(1).!)

  val typeVar: Parser[GenericVar] =
    P("T" ~ genericName ~ ";").map(GenericVar)

  val genericClassSigWithArgs: Parser[GenericClassName] =
    P(classNameSig ~ "<" ~ genericArgs ~ ">" ~ innerClassSig.rep ~ ";")
      .map((GenericClassName.apply _).tupled)

  val genericClassSigWithoutArgs: Parser[GenericClassName] =
    P(classNameSig ~ innerClassSig.rep ~ ";").map {
      case (className, innerClass) => GenericClassName(className, Seq.empty, innerClass)
    }
  val genericClassSig: Parser[GenericClassName] =
    P(genericClassSigWithArgs | genericClassSigWithoutArgs)

  val genericArraySig: Parser[GenericArray] =
    P("[" ~ (primitiveClassSig | genericClassSig | genericArraySig | typeVar))
      .map(GenericArray)


  val fieldTypeSignature: Parser[GenericSignature] =
    P(genericClassSig | genericArraySig | typeVar)

  val genericArgWithSignature: Parser[GenericArg] =
    P((lowerBoundary | upperBoundary).? ~ fieldTypeSignature)
      .map((GenericArg.apply _).tupled)

  val extendsObjectGenericArg =
    GenericArg(None, GenericClassName(ClassName.fromFqn("java.lang.Object")))

  val extendsObject: Parser[GenericArg] =
    P("*")
      .map(_ => extendsObjectGenericArg)

  val genericArgs: Parser[Seq[GenericArg]] =
    P((extendsObject | genericArgWithSignature).rep(1))

  val innerClassSigWithArgs: Parser[InnerClassName] =
    P("." ~ name ~ "<" ~ genericArgs ~ ">")
      .map((InnerClassName.apply _).tupled)

  val innerClassSigWithoutArgs: Parser[InnerClassName] =
    P("." ~ name)
      .map(InnerClassName(_, Seq.empty))

  val innerClassSig: Parser[InnerClassName] =
    P(innerClassSigWithArgs | innerClassSigWithoutArgs)

  val genericSigParam: Parser[GenericParam] =
    P(genericName ~ ":" ~ (":".? ~ fieldTypeSignature).rep(1))
      .map((GenericParam.apply _).tupled)

  val genericWithParam: Parser[GenericClass] =
    P("<" ~ genericSigParam.rep(1) ~ ">" ~ genericClassSig.rep(1))
      .map((GenericClass.apply _).tupled)

  val genericSuper: Parser[GenericClass] =
    P(genericClassSig.rep(1))
      .map(GenericClass(Seq.empty, _))

  val generic: Parser[GenericClass] =
    P((genericWithParam | genericSuper) ~ End)

  def parseGeneric(desc: String): Parsed.Failure Either GenericClass = {
    generic.parse(desc) match {
      case Parsed.Success(sig, _) => Right(sig)
      case f: Parsed.Failure => Left(f)
    }
  }
}
