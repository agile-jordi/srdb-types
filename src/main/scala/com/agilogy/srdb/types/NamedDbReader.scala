package com.agilogy.srdb.types

import java.sql.ResultSet

/**
 * A [[DbReader]] that reads columns by name, instead of reading them by position
 *
 * An instance of NamedDbReader can be obtained from a [[ColumnType]] using [[com.agilogy.srdb.types#notNull]] or [[com.agilogy.srdb.types#optional]].
 *
 * @tparam T The Scala class returned when reading from the `ResultSet`
 * @group API
 */
trait NamedDbReader[T] extends DbReader[T] {

  override def map[T2](f: (T) => T2): NamedDbReader[T2] = MappedNamedDbReader(this, f)
}

case class MappedNamedDbReader[T1, T2](source: NamedDbReader[T1], mf: T1 => T2) extends NamedDbReader[T2] {

  override def get(rs: ResultSet): T2 = mf(source.get(rs))

  override def map[T3](f: (T2) => T3): NamedDbReader[T3] = MappedNamedDbReader(source, mf.andThen(f))
}

trait AtomicNamedDbReader[T] extends NamedDbReader[T]

case class AtomicNotNullNamedDbReader[T](name: String)(implicit columnType: ColumnType[T]) extends AtomicNamedDbReader[T] {

  override def get(rs: ResultSet): T = columnType.get(rs, name).getOrElse(throw NullColumnReadException(name, rs))

  def optional: AtomicOptionalNamedDbReader[T] = AtomicOptionalNamedDbReader[T](name)
}

case class AtomicOptionalNamedDbReader[T](name: String)(implicit columnType: ColumnType[T]) extends AtomicNamedDbReader[Option[T]] {

  override def get(rs: ResultSet): Option[T] = columnType.get(rs, name)

  def notNull: AtomicNotNullNamedDbReader[T] = AtomicNotNullNamedDbReader(name)
}

trait CombinedNamedDbReader[T] extends NamedDbReader[T]

trait CombinedNotNullNamedDbReader[T] extends CombinedNamedDbReader[T] {
  def optional: CombinedOptionalNamedDbReader[T] = CombinedOptionalNamedDbReader(this)
}

case class CombinedOptionalNamedDbReader[T](notNull: CombinedNotNullNamedDbReader[T]) extends CombinedNamedDbReader[Option[T]] {

  override def get(rs: ResultSet): Option[T] = {
    try {
      Some(notNull.get(rs))
    } catch {
      case NullColumnReadException(_, _) => None
    }
  }

}