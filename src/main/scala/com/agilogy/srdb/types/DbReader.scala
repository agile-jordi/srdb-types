package com.agilogy.srdb.types

import java.sql.ResultSet

/**
 * A type class of readers of instances of type `T`.
 *
 * Given a `ResultSet`, a DbReader may read any number of columns and return an instance of `T`
 *
 * It MUST NOT call `ResultSet.next`
 *
 * A [[NotNullAtomicDbType]] and an [[OptionalAtomicDbType]], which are subclasses of [[DbReader]] are implicitly available for every [[ColumnType]]
 *
 * @tparam T The Scala class returned when reading from the `ResultSet`
 * @group API
 */
trait DbReader[T] extends (ResultSet => T) {

  self =>

  def get(rs: ResultSet): T

  def apply(rs: ResultSet): T = get(rs)

  //  def map[T2](f: T => T2): DbReader[T2]

}

/**
 * A [[DbReader]] that reads consecutive columns by position, instead of reading them by name
 *
 * A [[NotNullAtomicDbType]] and an [[OptionalAtomicDbType]], which are subclasses of [[PositionalDbReader]] are implicitly available for every [[ColumnType]]
 *
 * An instance of [[PositionalDbReader]]`[(T1,...,Tn)]` is available implicitly from [[PositionalDbReader]]`[T1]` to [[PositionalDbReader]]`[Tn]`.
 * An instance of [[DbType]]`[(T1,...,Tn)]` (which a subclass of [[PositionalDbReader]]`[(T1,...,Tn)]`)  is available implicitly from [[DbType]]`[T1]` to [[DbType]]`[Tn]`.
 *
 * @tparam T The Scala class returned when reading from the `ResultSet`
 * @group API
 */
trait PositionalDbReader[T] extends DbReader[T] {

  self =>

  type NotNull
  def optional: OptionalPositionalDbReader[NotNull]
  def notNull: NotNullPositionalDbReader[NotNull]

  protected val t0 = HasLength0

  val length: Int

  def get(rs: ResultSet): T = get(rs, 1)
  def get(rs: ResultSet, pos: Int): T

}

trait NotNullPositionalDbReader[T] extends PositionalDbReader[T] {

  self =>

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

case class DerivedOptionalPositionalDbReader[T](notNull: NotNullPositionalDbReader[T]) extends OptionalPositionalDbReader[T]

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

  type NotNull
  def notNull: NotNullNamedDbReader[NotNull]
  def optional: OptionalNamedDbReader[NotNull]

  override def get(rs: ResultSet): T

}

trait NotNullNamedDbReader[T] extends NamedDbReader[T] {

  self =>

  type NotNull = T
  def notNull: NotNullNamedDbReader[T] = this
  def optional: OptionalNamedDbReader[T] = DerivedOptionalNamedDbReader(this)

  def map[T2](f: (T) => T2): NotNullNamedDbReader[T2] = new NotNullNamedDbReader[T2] {

    override def get(rs: ResultSet): T2 = f(self.get(rs))

  }

}

trait OptionalNamedDbReader[T] extends NamedDbReader[Option[T]] {

  self =>

  type NotNull = T
  def notNull: NotNullNamedDbReader[T]
  def optional: OptionalNamedDbReader[T] = this

  override def get(rs: ResultSet): Option[T] = try {
    Some(notNull.get(rs))
  } catch {
    case ncre: NullColumnReadException => None
  }

  def map[T2](f: (T) => T2): OptionalNamedDbReader[T2] = new DerivedOptionalNamedDbReader[T2](notNull.map(f))

}

case class DerivedOptionalNamedDbReader[T](notNull: NotNullNamedDbReader[T]) extends OptionalNamedDbReader[T]

/**
 * A [[NamedDbReader]] that reads a single not null column and always returns a result
 *
 * An instance of [[NotNullAtomicNamedDbReader]] can be get from a [[ColumnType]] using [[com.agilogy.srdb.types.notNull]]
 *
 * @tparam T The Scala class returned when reading from the `ResultSet`
 * @group API
 */
case class NotNullAtomicNamedDbReader[T: ColumnType](name: String) extends NotNullNamedDbReader[T] {

  override def get(rs: ResultSet): T = implicitly[ColumnType[T]].get(rs, name).getOrElse(throw new NullColumnReadException)
}

/**
 * A [[NamedDbReader]] that reads a single nullable column and may return `Some` result or `None` if the column was null
 *
 * An instance of [[OptionalAtomicNamedReader]] can be get from a [[ColumnType]] using [[com.agilogy.srdb.types.optional]]
 *
 * @tparam T The Scala class returned when reading from the `ResultSet`
 * @group API
 */
case class OptionalAtomicNamedReader[T: ColumnType](name: String) extends OptionalNamedDbReader[T] {

  override def get(rs: ResultSet): Option[T] = implicitly[ColumnType[T]].get(rs, name)

  override def notNull: NotNullNamedDbReader[T] = NotNullAtomicNamedDbReader(name)
}