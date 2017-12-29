package ensime
package asm

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.Opcodes._
import java.io._
import scala.collection.JavaConverters._
import fastparse.all.Parsed

import cats._, cats.implicits._

object Visitor {
  def apply(in: InputStream): ClassNode = {
    val reader = new ClassReader(in)
    val n = new ClassNode()
    reader.accept(n, ClassReader.SKIP_FRAMES)
    n
  }
}

object InnerClassNode {
  def unapply(n: InnerClassNode): Option[(
    String,
    Option[String],
    Option[String],
    Int)] = {
    Some((
      n.name,
      Option(n.outerName),
      Option(n.innerName),
      n.access
    ))
  }
}

object TryCatchBlockNode {
  def unapply(n: TryCatchBlockNode): Option[(
    LabelNode,
    LabelNode,
    LabelNode,
    Option[String],
    List[TypeAnnotationNode],
    List[TypeAnnotationNode]
  )] = {
    Some((
      n.start,
      n.end,
      n.handler,
      Option(n.`type`),
      ensureList(n.visibleTypeAnnotations),
      ensureList(n.invisibleTypeAnnotations)
    ))
  }
}


object LineNumberNode {
  def unapply(n: AbstractInsnNode): Option[(Int, LabelNode)] = {
    if(n.isInstanceOf[LineNumberNode]) {
      val ln = n.asInstanceOf[LineNumberNode]
      Some((
        ln.line,
        ln.start
      ))
    } else None
  }
}

object FieldNode {
  def unapply(n: FieldNode): Option[(
    Int,
    String,
    String,
    Option[String],
    Option[Object],
    List[AnnotationNode],
    List[AnnotationNode],
    List[TypeAnnotationNode],
    List[TypeAnnotationNode],
    List[Attribute]
  )] = {
    Some((
      n.access,
      n.name,
      n.desc,
      Option(n.signature),
      Option(n.value),
      ensureList(n.visibleAnnotations),
      ensureList(n.invisibleAnnotations),
      ensureList(n.visibleTypeAnnotations),
      ensureList(n.invisibleTypeAnnotations),
      ensureList(n.attrs)
    ))
  }
}

object ClassNode {
  def unapply(n: ClassNode): Option[(
    Int,
    Int,
    String,
    Option[String],
    Option[String],
    List[String],
    Option[String],
    Option[String],
    Option[String],
    Option[String],
    Option[String],
    List[AnnotationNode],
    List[AnnotationNode],
    List[TypeAnnotationNode],
    List[TypeAnnotationNode],
    List[Attribute],
    List[InnerClassNode],
    List[FieldNode],
    List[MethodNode]
  )] = {
    Some((
      n.version,
      n.access,
      n.name,
      Option(n.signature),
      Option(n.superName),
      ensureList(n.interfaces),
      Option(n.sourceFile),
      Option(n.sourceDebug),
      Option(n.outerClass),
      Option(n.outerMethod),
      Option(n.outerMethodDesc),
      ensureList(n.visibleAnnotations),
      ensureList(n.invisibleAnnotations),
      ensureList(n.visibleTypeAnnotations),
      ensureList(n.invisibleTypeAnnotations),
      ensureList(n.attrs),
      ensureList(n.innerClasses),
      ensureList(n.fields),
      ensureList(n.methods)
    ))
  }
}

object MethodNode {
  def unapply(n: MethodNode)
      : Option[(
    Int,
    String,
    String,
    Option[String],
    List[String],
    List[ParameterNode],
    List[AnnotationNode],
    List[AnnotationNode],
    List[TypeAnnotationNode],
    List[TypeAnnotationNode],
    List[Attribute],
    Option[Object],
    List[List[AnnotationNode]],
    List[List[AnnotationNode]],
    List[AbstractInsnNode],
    List[TryCatchBlockNode],
    Int,
    Int,
    List[LocalVariableNode],
    List[LocalVariableAnnotationNode],
    List[LocalVariableAnnotationNode]
      )]
  = {
    Some((n.access,
      n.name,
      n.desc,
      Option(n.signature),
      ensureList(n.exceptions),
      ensureList(n.parameters),
      ensureList(n.visibleAnnotations),
      ensureList(n.invisibleAnnotations),
      ensureList(n.visibleTypeAnnotations),
      ensureList(n.invisibleTypeAnnotations),
      ensureList(n.attrs),
      Option(n.annotationDefault),
      ensureArray(n.visibleParameterAnnotations),
      ensureArray(n.invisibleParameterAnnotations),
      n.instructions.toArray.toList,
      ensureList(n.tryCatchBlocks),
      n.maxStack,
      n.maxLocals,
      ensureList(n.localVariables),
      ensureList(n.visibleLocalVariableAnnotations),
      ensureList(n.invisibleLocalVariableAnnotations)
    ))
  }
}

object MultiANewArrayInsnNode {
  def unapply(n: AbstractInsnNode): Option[(String, Int)] = {
    if(n.isInstanceOf[MultiANewArrayInsnNode]) {
      val nn = n.asInstanceOf[MultiANewArrayInsnNode]
      Some((nn.desc, nn.dims))
    } else None
  }
}
object TypeInsnNode {
  def unapply(n: AbstractInsnNode): Option[(String)] = {
    if(n.isInstanceOf[TypeInsnNode]) {
      val nn = n.asInstanceOf[TypeInsnNode]
      Some((nn.desc))
    } else None
  }
}
object FieldInsnNode {
  def unapply(n: AbstractInsnNode): Option[(String, String, String)] = {
    if(n.isInstanceOf[FieldInsnNode]) {
      val nn = n.asInstanceOf[FieldInsnNode]
      Some((nn.desc, nn.name, nn.owner))
    } else None
  }
}
object MethodInsnNode {
  def unapply(n: AbstractInsnNode): Option[(String, Boolean, String, String)] = {
    if(n.isInstanceOf[MethodInsnNode]) {
      val nn = n.asInstanceOf[MethodInsnNode]
      Some((nn.desc, nn.itf, nn.name, nn.owner))
    } else None
  }
}
object InvokeDynamicInsnNode {

  def unapply(n: AbstractInsnNode): Option[(String, String, Handle)] = {
    if(n.isInstanceOf[InvokeDynamicInsnNode]) {
      val nn = n.asInstanceOf[InvokeDynamicInsnNode]
      Some((nn.desc, nn.name, nn.bsm))
    } else None
  }
}

object Domain {

  def field(className: ClassName)(n: FieldNode): Parsed.Failure Either RawField = n match {
    case FieldNode(a, n, d, sig, _, vas, ivas, vtas, ivtas, _) =>
      val refs = (d :: ivas.map(_.desc) ++ vas.map(_.desc) ++ vtas.map(_.desc) ++ ivtas.map(_.desc)).traverse(d => DescriptorType.from(d).map(ClassName.fromDescriptorType)).map(_.toSet)
      for {
        rs <- refs
        d <- DescriptorType.from(d)
      } yield RawField(
        FieldName(className, n),
        d,
        sig,
        Access(a),
        rs)
  }

  def method(className: ClassName)(n: MethodNode): Parsed.Failure Either RawMethod = n match {
    case MethodNode(a, n, d, sig, _, _, vas, ivas, vtas, ivtas, _, _, vpas, ivpas, insns, tcbs, _, _, lvs, vlvas, ivlvas) =>
      val refs = (d :: vas.map(_.desc) ++ ivas.map(_.desc) ++ vtas.map(_.desc) ++ ivtas.map(_.desc) ++ vpas.flatten.map(_.desc) ++ ivpas.flatten.map(_.desc) ++
        tcbs.filter {
          case TryCatchBlockNode(_, _, _, tpe, _, _) => tpe.nonEmpty
        }.map(_.`type`) ++
        lvs.map(_.desc) ++ vlvas.map(_.desc) ++ ivlvas.map(_.desc)
      ).map(ClassName.fromInternal).toSet

      def memberOrInit(owner: String, name: String): MemberName = {
        name match {
          case "<init>" | "<clinit>" => ClassName.fromInternal(owner)
          case member => FieldName(ClassName.fromInternal(owner), member)
        }
      }

      val insnRefs = insns.flatTraverse {
        case MultiANewArrayInsnNode(desc, _) => DescriptorType.from(desc).map(ClassName.fromDescriptorType).map(List(_))
        case TypeInsnNode(desc) => Right(List(ClassName.fromInternal(desc)))
        case FieldInsnNode(desc, name, owner) => Right(List(memberOrInit(owner, name)))
        case MethodInsnNode(desc, _, name, owner) =>
          val ownerName = ClassName.fromInternal(owner)
          val d = DescriptorParser.parse(desc)
          d.map(d => ownerName :: MethodName(ownerName, name, d) :: d.classes)
        case InvokeDynamicInsnNode(_, _, bsm) =>
          memberOrInit(bsm.getOwner, bsm.getName)
          DescriptorParser.parse(bsm.getDesc).map(_.classes)
      }.map(_.toSet)
      for {
        d <- Descriptor.from(d)
        irefs <- insnRefs
      } yield RawMethod(
        MethodName(className, n, d),
        Access(a),
        sig,
        None,
        refs ++ irefs)
  }

  def apply(n: ClassNode): Parsed.Failure Either RawClassfile = n match {
    case ClassNode(_, a, n, sig, sup, is, sf, _, _, _, _, vas, ivas, vtas, ivtas, _, ics, fs, ms) =>
      val className = ClassName.fromInternal(n)
      val innerClasses = ics.filter {
        case InnerClassNode(_, on, _, _) => 
          on.map(ClassName.fromInternal).exists(_ == className)
      }.map(n => ClassName.fromInternal(n.name)).toSet
      val interfaces = is.map(ClassName.fromInternal)
      val refs = (vas.map(_.desc) ++ ivas.map(_.desc) ++ vtas.map(_.desc) ++ ivtas.map(_.desc) ++ sup.toList).map(ClassName.fromInternal).toSet ++ interfaces

      val signature = sig.filter(_.nonEmpty).flatMap(s => SignatureParser.parseGeneric(s).toOption)

      val fields = fs.traverse(field(className))
      val methods = ms.traverse(method(className))
      for {
        ms <- methods
        fs <- fields
      } yield RawClassfile(
        className,
        generics = signature,
        innerClasses,
        sup.map(ClassName.fromInternal),
        interfaces,
        Access(a),
        Access.isDeprecated(a),
        fs,
        ms,
        RawSource(sf, None),
        sf.exists(_.endsWith(".scala")),
        refs)
  }
}


object Test {
  val baseDir = "/home/zainab/coding/ithaca/fs2-reactive-streams/core/target/scala-2.11/classes/fs2/interop/reactivestreams/"
  val filenames = List("package.class", "StreamSubscriber.class", "StreamSubscriber$.class")

  val clazzes = filenames.map(baseDir + _).map{ n =>
    val is = new FileInputStream(new File(n))
    val clazz = Visitor(is)
    is.close()
    clazz
  }
  def main(args: Array[String]): Unit = {
    println(clazzes.head)
  }
}
