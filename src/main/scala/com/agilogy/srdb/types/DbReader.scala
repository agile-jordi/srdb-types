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

  def map[T2](f: T => T2): DbReader[T2]

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

  protected val t0 = HasLength0

  val length: Int

  def get(rs: ResultSet): T = get(rs, 1)
  def get(rs: ResultSet, pos: Int): T

  override def map[T2](f: (T) => T2): PositionalDbReader[T2] = new PositionalDbReader[T2] {

    override val length: Int = self.length

    override def get(rs: ResultSet): T2 = f(self.get(rs))
    override def get(rs: ResultSet, pos: Int): T2 = f(self.get(rs, pos))
  }
}

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

  override def get(rs: ResultSet): T

  override def map[T2](f: (T) => T2): NamedDbReader[T2] = new NamedDbReader[T2] {

    override def get(rs: ResultSet): T2 = f(self.get(rs))

  }
}

/**
 * A [[NamedDbReader]] that reads a single not null column and always returns a result
 *
 * An instance of [[NotNullAtomicNamedDbReader]] can be get from a [[ColumnType]] using [[com.agilogy.srdb.types.notNull]]
 *
 * @tparam T The Scala class returned when reading from the `ResultSet`
 * @group API
 */
case class NotNullAtomicNamedDbReader[T: ColumnType](name: String) extends NamedDbReader[T] {

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
case class OptionalAtomicNamedReader[T: ColumnType](name: String) extends NamedDbReader[Option[T]] {

  override def get(rs: ResultSet): Option[T] = implicitly[ColumnType[T]].get(rs, name)
}