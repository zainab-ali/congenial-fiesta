package ensime
package asm

import scala.collection.immutable.Queue
import org.objectweb.asm.Opcodes._
import fastparse.all.Parsed

sealed trait Access

object Access {

  case object Public extends Access
  case object Default extends Access
  case object Protected extends Access
  case object Private extends Access

  def apply(code: Int): Access =
    if ((ACC_PUBLIC & code) > 0) Public
    else if ((ACC_PROTECTED & code) > 0) Protected
    else if ((ACC_PRIVATE & code) > 0) Private
    else Default

  def isDeprecated(code: Int): Boolean =
    (ACC_DEPRECATED & code) > 0
}

case class PackageName(path: List[String]) {
  def parent = PackageName(path.dropRight(1))
}

case class ClassName(pack: PackageName, name: String) extends DescriptorType with MemberName {
  def isPrimitive: Boolean = pack == ClassName.root
}

object ClassName {
  private val root = PackageName(Nil)
  // we consider primitives to be ClassNames
  private def primitive(name: String): ClassName = ClassName(root, name)

  val primitiveBoolean = primitive("boolean")
  val primitiveByte = primitive("byte")
  val primitiveChar = primitive("char")
  val primitiveShort = primitive("short")
  val primitiveInt = primitive("int")
  val primitiveLong = primitive("long")
  val primitiveFloat = primitive("float")
  val primitiveDouble = primitive("double")
  val primitiveVoid = primitive("void")

  // must be a single type descriptor
  // strips array reification
  def fromDescriptorType(desc: DescriptorType): ClassName =
    desc match {
      case c: ClassName => c
      case a: ArrayDescriptor => a.fqn
    }

  // internal name is effectively the FQN with / instead of dots
  def fromInternal(internal: String): ClassName = fromFqn(internal, '/')

  def fromFqn(internal: String, splitter: Char = '.'): ClassName = {
    val parts = internal.split(splitter).toList
    val (before, after) = parts.splitAt(parts.size - 1)
    ClassName(PackageName(before), after.head)
  }

}

sealed trait DescriptorType
object DescriptorType {
  def from(s: String): Parsed.Failure Either DescriptorType = DescriptorParser.parseType(s)
}
final case class Descriptor(params: List[DescriptorType], ret: DescriptorType) {
  def classes: List[ClassName] = (ret :: params).map(ClassName.fromDescriptorType)
}

object Descriptor {
  def from(s: String): Parsed.Failure Either Descriptor = DescriptorParser.parse(s)
}

sealed trait MemberName

// not always available in the ASM parser
case class FieldName(
    owner: ClassName,
    name: String
) extends MemberName

// FQNs are not really unique, because method overloading, so fudge
// the descriptor into the FQN
final case class MethodName(
    owner: ClassName,
    name: String,
    descriptor: Descriptor
) extends MemberName

// Generics signature
sealed trait GenericSignature

sealed trait BoundType
case object UpperBound extends BoundType
case object LowerBound extends BoundType

final case class GenericClass(
  genericParam: Seq[GenericParam],
  superClasses: Seq[GenericClassName]
)

final case class GenericParam(
  name: String,
  classNames: Seq[GenericSignature]
)

final case class GenericClassName(
  className: ClassName,
  genericArg: Seq[GenericArg] = Seq.empty,
  innerClass: Seq[InnerClassName] = Seq.empty
) extends GenericSignature

final case class InnerClassName(
  name: String,
  genericArg: Seq[GenericArg] = Seq.empty
)

final case class GenericArg(
  boundType: Option[BoundType],
  genericSignature: GenericSignature
)

final case class GenericArray(className: GenericSignature)
  extends GenericSignature

final case class GenericVar(name: String)
  extends GenericSignature

final case class ArrayDescriptor(depth: Int, fqn: ClassName) extends DescriptorType

final case class RawClassfile(
    name: ClassName,
    generics: Option[GenericClass],
    innerClasses: Set[ClassName],
    superClass: Option[ClassName],
    interfaces: List[ClassName],
    access: Access,
    deprecated: Boolean,
    fields: List[RawField],
    methods: List[RawMethod],
    source: RawSource,
    isScala: Boolean,
    internalRefs: Set[ClassName]
)

final case class RawSource(
  filename: Option[String],
  line: Option[Int]
)

sealed trait InternalRef

final case class RawField(
    name: FieldName,
    clazz: DescriptorType,
    generics: Option[String],
    access: Access,
    internalRefs: Set[ClassName]
)

final case class RawMethod(
    name: MethodName,
    access: Access,
    generics: Option[String],
    line: Option[Int],
    internalRefs: Set[MemberName]
)
