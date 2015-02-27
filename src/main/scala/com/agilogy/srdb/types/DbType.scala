package com.agilogy.srdb.types

import java.sql.{PreparedStatement, ResultSet}

private[types] case object HasLength0{
  val length:Int = 0
}

trait DbType[T] extends PositionalDbReader[T] with DbWriter[T]{
  protected val t0 = HasLength0
}

case class NotNullAtomicDbType[T](implicit columnType:ColumnType[T]) extends DbType[T] {

  override val length: Int = 1

  override def get(rs: ResultSet, pos: Int): T = columnType.get(rs,pos).getOrElse(throw new NullColumnReadException)

  override def set(ps: PreparedStatement, pos: Int, value: T): Unit = columnType.set(ps,pos,Some(value))

}

case class OptionalAtomicDbType[T](implicit columnType: ColumnType[T]) extends DbType[Option[T]]{

  override val length: Int = 1

  override def get(rs: ResultSet, pos: Int): Option[T] = columnType.get(rs,pos)

  override def set(ps: PreparedStatement, pos: Int, value: Option[T]): Unit = columnType.set(ps,pos, value)

}


