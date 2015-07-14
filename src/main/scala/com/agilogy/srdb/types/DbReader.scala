package com.agilogy.srdb.types

import java.sql.ResultSet

/**
 * A type class of readers of instances of type `T`.
 *
 * Given a `ResultSet`, a DbReader may read any number of columns and return an instance of `T`
 *
 * It MUST NOT call `ResultSet.next`
 *
 * A [[NotNullDbType]] and an [[OptionalDbType]], which are subclasses of [[DbReader]] are implicitly available for every [[ColumnType]]
 *
 * @tparam T The Scala class returned when reading from the `ResultSet`
 * @group API
 */
trait DbReader[T] extends (ResultSet => T) {

  self =>

  type Id[+X] = X
  type MT[_]
  type NotNull

  def get(rs: ResultSet): T

  def apply(rs: ResultSet): T = get(rs)

  def map[T2](f: NotNull => T2): DbReader[MT[T2]]

}

/**
 * A [[DbReader]] that reads consecutive columns by position, instead of reading them by name
 *
 * A [[NotNullDbType]] and an [[OptionalDbType]], which are subclasses of [[PositionalDbReader]] are implicitly available for every [[ColumnType]]
 *
 * An instance of [[PositionalDbReader]]`[(T1,...,Tn)]` is available implicitly from [[PositionalDbReader]]`[T1]` to [[PositionalDbReader]]`[Tn]`.
 * An instance of [[DbType]]`[(T1,...,Tn)]` (which a subclass of [[PositionalDbReader]]`[(T1,...,Tn)]`)  is available implicitly from [[DbType]]`[T1]` to [[DbType]]`[Tn]`.
 *
 * @tparam T The Scala class returned when reading from the `ResultSet`
 * @group API
 */
trait PositionalDbReader[T] extends DbReader[T] {

  self =>

  def optional: OptionalPositionalDbReader[NotNull]
  def notNull: NotNullPositionalDbReader[NotNull]

  protected val t0 = HasLength0

  val length: Int

  def get(rs: ResultSet): T = get(rs, 1)
  def get(rs: ResultSet, pos: Int): T

  def map[T2](f: NotNull => T2): PositionalDbReader[MT[T2]]

}

trait NotNullPositionalDbReader[T] extends PositionalDbReader[T] {

  self =>

  type MT[X] = Id[X]
  type NotNull = T
  def optional: OptionalPositionalDbReader[T] = DerivedOptionalPositionalDbReader(this)
  def notNull: NotNullPositionalDbReader[T] = this

  def map[T2](f: (T) => T2): NotNullPositionalDbReader[T2] = new NotNullPositionalDbReader[T2] {

    override val length: Int = self.length

    override def get(rs: ResultSet): T2 = f(self.get(rs))
    override def get(rs: ResultSet, pos: Int): T2 = f(self.get(rs, pos))
  }

}

trait OptionalPositionalDbReader[T] extends PositionalDbReader[Option[T]] {

  self =>

  type MT[X] = Option[X]
  type NotNull = T
  def optional: OptionalPositionalDbReader[T] = this
  def notNull: NotNullPositionalDbReader[T]

  override val length: Int = notNull.length

  override def get(rs: ResultSet, pos: Int): Option[T] = try {
    Some(notNull.get(rs, pos))
  } catch {
    case e: NullColumnReadException => None
  }

  def map[T2](f: (T) => T2): OptionalPositionalDbReader[T2] = new DerivedOptionalPositionalDbReader[T2](notNull.map(f))
}

private[types] case class DerivedOptionalPositionalDbReader[T](notNull: NotNullPositionalDbReader[T]) extends OptionalPositionalDbReader[T]

/**
 * A [[DbReader]] that reads columns by name, instead of reading them by position
 *
 * An instance of NamedDbReader can be obtained from a [[ColumnType]] using [[com.agilogy.srdb.types#notNull]] or [[com.agilogy.srdb.types#optional]].
 *
 * @tparam T The Scala class returned when reading from the `ResultSet`
 * @group API
 */
trait NamedDbReader[T] extends DbReader[T] {

  self =>

  def notNull: NotNullNamedDbReader[NotNull]
  def optional: OptionalNamedDbReader[NotNull]

  override def get(rs: ResultSet): T

}

trait NotNullNamedDbReader[T] extends NamedDbReader[T] {

  self =>

  type MT[X] = X
  type NotNull = T
  def notNull: NotNullNamedDbReader[T] = this
  def optional: OptionalNamedDbReader[T] = OptionalNamedDbReader(this)

  def map[T2](f: (T) => T2): NotNullNamedDbReader[T2] = new NotNullNamedDbReader[T2] {

    override def get(rs: ResultSet): T2 = f(self.get(rs))

  }

}

case class OptionalNamedDbReader[T](notNull: NotNullNamedDbReader[T]) extends NamedDbReader[Option[T]] {

  self =>

  type NotNull = T
  type MT[X] = Option[X]
  def optional: OptionalNamedDbReader[T] = this

  override def get(rs: ResultSet): Option[T] = try {
    Some(notNull.get(rs))
  } catch {
    case ncre: NullColumnReadException => None
  }

  def map[T2](f: (T) => T2): OptionalNamedDbReader[T2] = OptionalNamedDbReader[T2](notNull.map(f))

}

///**
// * A [[NamedDbReader]] that reads a single nullable column and may return `Some` result or `None` if the column was null
// *
// * An instance of [[OptionalAtomicNamedReader]] can be get from a [[ColumnType]] using [[com.agilogy.srdb.types.optional]]
// *
// * @tparam T The Scala class returned when reading from the `ResultSet`
// * @group API
// */
//case class OptionalAtomicNamedReader[T: ColumnType](name: String) extends OptionalNamedDbReader[T] {
//
//  override def get(rs: ResultSet): Option[T] = implicitly[ColumnType[T]].get(rs, name)
//
//  override def notNull: NotNullNamedDbReader[T] = NotNullAtomicNamedDbReader(name)
//}