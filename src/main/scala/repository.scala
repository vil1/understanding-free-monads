package repository

import cats.free.Free
import cats.~>

import scala.collection.mutable
import scala.language.higherKinds

/**
  * @author Valentin Kasas
  */
sealed trait RepoF[Id, A] {
  def id: Id
}
object RepoF {

  final case class Exists[Id](id: Id)                     extends RepoF[Id, Boolean]
  final case class Get[Id, A](id: Id)                     extends RepoF[Id, Option[A]]
  final case class Store[Id, A](id: Id, value: A)         extends RepoF[Id, Unit]
  final case class Update[Id, A](id: Id, transfo: A => A) extends RepoF[Id, A]
  final case class Delete[Id](id: Id)                     extends RepoF[Id, Unit]
}


trait Repository[Id, Entity] {

  import RepoF._

  import scala.language.implicitConversions
  type RepoId[A] = RepoF[Id, A]
  type Repo[A]   = Free[RepoId, A]

  private implicit def liftRepoF[A](r: RepoId[A]): Repo[A] = Free.liftF(r)

  def exists(id: Id): Repo[Boolean]     = Exists(id)
  def get(id: Id): Repo[Option[Entity]] = Get[Id, Entity](id)
  def getOrElse(id: Id, default: Entity): Repo[Entity] =
    for {
      opt <- get(id)
    } yield opt.getOrElse(default)

  def put(id: Id, entity: Entity): Repo[Option[Entity]] =
    for {
      _      <- Store(id, entity)
      result <- Get[Id, Entity](id)
    } yield result

  def delete(id: Id): Repo[Unit] = Delete(id)

  def unsafeGet(id: Id): Repo[Entity] = Get[Id, Entity](id).map(_.get)

  def unsafeUpdate(id: Id, f: Entity => Entity): Repo[Entity] =
    for {
      entity <- unsafeGet(id)
      updated = f(entity)
      _ <- Store(id, updated)
    } yield updated

  def update(id: Id, transfo: Entity => Entity): Repo[Entity] = Update(id, transfo)
}

trait InMemRepositoryInterpreter[Id, Entity] {

  type RepoId[A] = RepoF[Id, A]
  type Repo[A]   = Free[RepoId, A]

  import RepoF._

  def trans(store: mutable.Map[Id, Entity]) = new (RepoId ~> cats.Id) {
    override def apply[A](program: RepoId[A]): cats.Id[A] = program match {
      case Exists(id:Id @unchecked) => store.contains(id).asInstanceOf[A]
      case Get(id:Id @unchecked) => store.get(id).asInstanceOf[A]
      case Store(id:Id @unchecked, value: Entity @unchecked) => store.put(id, value).asInstanceOf[A]
      case Update(id:Id @unchecked, transfo: Function1[Entity, Entity] @unchecked) => store.update(id, transfo(store(id))).asInstanceOf[A]
      case Delete(id:Id @unchecked) => store.remove(id).asInstanceOf[A]
    }
  }

}
