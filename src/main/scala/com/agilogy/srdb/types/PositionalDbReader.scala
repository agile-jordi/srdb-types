package com.agilogy.srdb.types

import java.sql.ResultSet

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

  protected val t0 = HasLength0

  val length: Int

  def get(rs: ResultSet): T = get(rs, 1)
  def get(rs: ResultSet, pos: Int): T

  override def map[T2](f: (T) => T2): PositionalDbReader[T2] = MappedPositionalDbReader(this, f)
}

case class MappedPositionalDbReader[T1, T2](source: PositionalDbReader[T1], mf: T1 => T2) extends PositionalDbReader[T2] {

  override val length: Int = source.length

  override def get(rs: ResultSet, pos: Int): T2 = mf(source.get(rs, pos))

  override def map[T3](f: T2 => T3): PositionalDbReader[T3] = MappedPositionalDbReader[T1, T3](source, mf.andThen(f))

}

//trait NotNullPositionalDbReader[T] extends PositionalDbReader[T] with NotNullDbReader[T] {
//  override def optional: OptionalPositionalDbReader[T]
//}
//
//trait OptionalPositionalDbReader[T] extends PositionalDbReader[Option[T]] with OptionalDbReader[T] {
//  override def notNull: NotNullPositionalDbReader[T]
//}

trait AtomicPositionalDbReader[T] extends PositionalDbReader[T] {
  override val length: Int = 1
  type NotNull
  type Optional
  def as(name: String): AtomicNamedDbReader[T]
}

case class AtomicNotNullPositionalDbReader[T]()(implicit columnType: ColumnType[T]) extends AtomicPositionalDbReader[T] {

  override type NotNull = T
  override type Optional = Option[T]

  override def get(rs: ResultSet, pos: Int): T = columnType.get(rs, pos).getOrElse(throw NullColumnReadException(pos.toString, rs))

  def optional: AtomicOptionalPositionalDbReader[T] = AtomicOptionalPositionalDbReader[T]

  override def as(name: String): AtomicNamedDbReader[T] = AtomicNotNullNamedDbReader[T](name)
}

case class AtomicOptionalPositionalDbReader[T]()(implicit columnType: ColumnType[T]) extends AtomicPositionalDbReader[Option[T]] {

  override type NotNull = T
  override type Optional = Option[T]

  override def get(rs: ResultSet, pos: Int): Option[T] = columnType.get(rs, pos)

  def notNull: AtomicNotNullPositionalDbReader[T] = AtomicNotNullPositionalDbReader[T]

  override def as(name: String): AtomicNamedDbReader[Option[T]] = AtomicOptionalNamedDbReader[T](name)
}

trait CombinedPositionalDbReader[T] extends PositionalDbReader[T]

trait CombinedNotNullPositionalDbReader[T] extends CombinedPositionalDbReader[T] {
  def optional: CombinedOptionalPositionalDbReader[T] = CombinedOptionalPositionalDbReader(this)
}

case class CombinedOptionalPositionalDbReader[T](notNull: CombinedNotNullPositionalDbReader[T]) extends CombinedPositionalDbReader[Option[T]] {

  override val length: Int = notNull.length

  override def get(rs: ResultSet, pos: Int): Option[T] = {
    try {
      Some(notNull.get(rs, pos))
    } catch {
      case NullColumnReadException(_, _) => None
    }
  }
}