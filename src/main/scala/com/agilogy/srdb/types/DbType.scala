package com.agilogy.srdb.types

import java.sql.{PreparedStatement, ResultSet}

private[types] case object HasLength0{
  val length:Int = 0
}

sealed trait DbType[T] extends DbReader[T] with DbWriter[T]{
  protected val t0 = HasLength0
}

trait PositionalDbType[T] extends PositionalDbReader[T] with DbType[T]

case class NotNullAtomicDbType[T](implicit columnType:ColumnType[T]) extends PositionalDbType[T] {

  override val length: Int = 1

  override def get(rs: ResultSet, pos: Int): T = columnType.get(rs,pos).getOrElse(throw new NullColumnReadException)

  override def set(ps: PreparedStatement, pos: Int, value: T): Unit = columnType.set(ps,pos,Some(value))

  def apply(name:String): NotNullAtomicNamedDbReader[T] = NotNullAtomicNamedDbReader(name)
}

case class OptionalAtomicDbType[T](implicit columnType: ColumnType[T]) extends PositionalDbType[Option[T]]{

  override val length: Int = 1

  override def get(rs: ResultSet, pos: Int): Option[T] = columnType.get(rs,pos)

  override def set(ps: PreparedStatement, pos: Int, value: Option[T]): Unit = columnType.set(ps,pos, value)

  def apply(name:String): OptionalAtomicNamedReader[T] = OptionalAtomicNamedReader(name)
}


