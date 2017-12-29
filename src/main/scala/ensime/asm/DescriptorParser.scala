package ensime
package asm

import fastparse.all._

object DescriptorParser {

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

  val packageName: Parser[PackageName] = P((((!CharIn(";/ ")).rep(1).! ~ "/")rep).map(seq => PackageName(seq.toList)))
  val name: Parser[String] = P((!CharIn(";/ ")).rep(1).!)
  val className: Parser[ClassName] = P("L" ~ packageName ~ name ~ ";").map((ClassName.apply _).tupled)

  val array: Parser[ArrayDescriptor] = P("[".!.rep.map(_.length) ~ P(className | primitiveType)).map((ArrayDescriptor.apply _).tupled)

  val tpe: Parser[DescriptorType] =
    P(className | boolean | byte | char | short | int | long | float | double | void | array)

  val descriptor: Parser[Descriptor] =
    P("(" ~ tpe.rep ~ ")" ~ tpe ~ End).map {
      case (paramSeq: Seq[DescriptorType], retType: DescriptorType) => Descriptor(paramSeq.toList, retType)
    }

  def parse(s: String): Parsed.Failure Either Descriptor = 
    descriptor.parse(s) match {
      case Parsed.Success(d, _) => Right(d)
      case f: Parsed.Failure => Left(f)
    }

  def parseType(s: String): Parsed.Failure Either DescriptorType = {
    tpe.parse(s) match {
      case Parsed.Success(d, _) => Right(d)
      case f: Parsed.Failure => Left(f)
    }
  }
}
