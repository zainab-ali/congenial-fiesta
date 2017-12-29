package ensime

import scala.collection.JavaConverters._
package object asm {

    def ensureList[A](l: java.util.List[_]): List[A] = {
      if(l == null) Nil else l.asScala.toList.asInstanceOf[List[A]]
    }

    def ensureArray[A](l: Array[java.util.List[_]]): List[List[A]] = {
      if(l == null) Nil else l.toList.map(ensureList)
    }
}
