package com.agilogy.srdb.types

import java.sql.{ PreparedStatement, ResultSet }

private[types] case object HasLength0 {
  val length: Int = 0
}

/**
 * A DbType is a combination of a [[PositionalDbReader]] and a [[DbWriter]]
 *
 * A [[NotNullAtomicDbType]] and an [[OptionalAtomicDbType]] are implicitly available for every [[ColumnType]]
 *
 * An instance of [[DbType]]`[(T1,...,Tn)]` is available implicitly from [[DbType]]s for T1 to Tn.
 *
 * @tparam T The Scala type represeing values read from the `ResultSet` or values to be written to a `PreparedStatement`
 * @group API
 */
trait DbType[T] extends PositionalDbReader[T] with DbWriter[T] {

  def xmap[NN2](f: NotNull => NN2, xf: NN2 => NotNull): DbType[MT[NN2]]
}

trait NotNullDbType[T] extends DbType[T] with NotNullPositionalDbReader[T] {

  self =>

  def xmap[T2](f: T => T2, xf: T2 => T): NotNullDbType[T2] = new NotNullDbType[T2] {

    override def get(rs: ResultSet, pos: Int): T2 = f(self.get(rs, pos))

    override def set(ps: PreparedStatement, pos: Int, value: T2): Unit = self.set(ps, pos, xf(value))

    override val length: Int = self.length
  }
}

trait OptionalDbType[T] extends DbType[Option[T]] with OptionalPositionalDbReader[T] {

  self =>

  def xmap[T2](f: T => T2, xf: T2 => T): OptionalDbType[T2] = new OptionalDbType[T2] {

    override def notNull: NotNullPositionalDbReader[T2] = self.notNull.map(f)

    override def set(ps: PreparedStatement, pos: Int, value: Option[T2]): Unit = self.set(ps, pos, value.map(xf))

  }

}

/**
 * A [[DbType]] mapping a single not null column and a single not null parameter
 *
 * A [[NotNullAtomicDbType]] is implicitly available for every [[ColumnType]]
 *
 * @param columnType `implicit` The column type for this instance
 * @tparam T The Scala type represeing values read from the `ResultSet` or values to be written to a `PreparedStatement`
 * @group API
 */
case class NotNullAtomicDbType[T](implicit columnType: ColumnType[T]) extends NotNullDbType[T] {

  override val length: Int = 1

  override def get(rs: ResultSet, pos: Int): T = columnType.get(rs, pos).getOrElse(throw new NullColumnReadException)

  override def set(ps: PreparedStatement, pos: Int, value: T): Unit = columnType.set(ps, pos, Some(value))

  //  override def optional: OptionalAtomicDbType[T] = OptionalAtomicDbType()
}

/**
 * A [[DbType]] mapping a single nullable column and a single optional parameter
 *
 * An [[OptionalAtomicDbType]] is implicitly available for every [[ColumnType]]
 *
 * @param columnType `implicit` The column type for this instance
 * @tparam T The Scala type represeing values read from the `ResultSet` or values to be written to a `PreparedStatement`
 * @group API
 */
case class OptionalAtomicDbType[T](implicit columnType: ColumnType[T]) extends OptionalDbType[T] {

  override val length: Int = 1

  override def get(rs: ResultSet, pos: Int): Option[T] = columnType.get(rs, pos)

  override def set(ps: PreparedStatement, pos: Int, value: Option[T]): Unit = columnType.set(ps, pos, value)

  override def notNull: NotNullDbType[T] = NotNullAtomicDbType[T]()
}

