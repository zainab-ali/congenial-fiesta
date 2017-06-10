package ensime

import java.nio.file._
import fs2.Stream

import cats._
import cats.effect._
import cats.implicits._

// sealed trait WEvent[A]

// case class Add[A](a: A) extends WEvent[A]
// case class Delete[A](a: A) extends WEvent[A]
// case class Modify[A](a: A) extends WEvent[A]

// //Should probably define the edit (does ensime just this for refactoring?)
// trait Watcher[F[_], A] {
//   def watch(a: A, f: Stream[F, A => Boolean]): Stream[F, WEvent[A]]
// }

// object Watcher {

//   def apply[F[_] : Async](p: Path): Watcher[F, Path] = {
//     val watcher = FileSystsms.getDefault.newWatchService
//     val key = p.register(
//       watcher,
//       StandardWatchEventKinds.ENTRY_CREATE,
//       StandardWatchEventKinds.ENTRY_MODIFY,
//       StandardWatchEventKinds.ENTRY_DELETE
//     )
//     new Watcher {
//       def watch(p: Path, f: Stream[F, A => Boolean]): Stream[F, WEvent[Path]] = {
        
//       }
//     }
//   }
// }

// //depedent type
// trait Load[F[_], A, B] {
//   def load(a: A): F[A]
// }

sealed trait CRUD[A]

object CRUD {
  case class Create[A](a: A) extends CRUD[A]
  case class Update[A](a: A) extends CRUD[A]
  case class Delete[A](a: A) extends CRUD[A]
}

trait VFS[F[_]] {
  def watch(p: Path): Stream[F, CRUD[Path]]
}

object VFS {
  def apply[F[_] : Async](): VFS[F] = new VFS[F] {
    def watch(p: Path): Stream[F, CRUD[Path]] = ???
  }
}


