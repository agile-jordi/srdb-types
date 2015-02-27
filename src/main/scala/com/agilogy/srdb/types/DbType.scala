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

  def apply(name:String): NotNullNamedAtomicDbType[T] = NotNullNamedAtomicDbType(name)
}

case class OptionalAtomicDbType[T](implicit columnType: ColumnType[T]) extends PositionalDbType[Option[T]]{

  override val length: Int = 1

  override def get(rs: ResultSet, pos: Int): Option[T] = columnType.get(rs,pos)

  override def set(ps: PreparedStatement, pos: Int, value: Option[T]): Unit = columnType.set(ps,pos,(value))

  def apply(name:String): OptionalNamedAtomicDbType[T] = OptionalNamedAtomicDbType(name)
}


trait NamedDbType[T] extends NamedDbReader[T] with DbType[T]

case class NotNullNamedAtomicDbType[T:ColumnType](name:String) extends NamedDbType[T] {

  override def get(rs: ResultSet): T = implicitly[ColumnType[T]].get(rs,name).getOrElse(throw new NullColumnReadException)

  override val length: Int = 1

  override def set(ps: PreparedStatement, pos: Int, value: T): Unit = implicitly[ColumnType[T]].set(ps,pos,Some(value))
}

case class OptionalNamedAtomicDbType[T:ColumnType](name:String) extends NamedDbType[Option[T]]{

  override def get(rs: ResultSet): Option[T] = implicitly[ColumnType[T]].get(rs,name)

  override val length: Int = 1

  override def set(ps: PreparedStatement, pos: Int, value: Option[T]): Unit = implicitly[ColumnType[T]].set(ps,pos,value)
}

trait DbTypeImplicits extends DbTypeCombinators {
  
  implicit def notNull[T:ColumnType]:NotNullAtomicDbType[T] = NotNullAtomicDbType[T]

  def notNull[T:ColumnType](name:String):NotNullNamedAtomicDbType[T] = NotNullAtomicDbType[T].apply(name)

  implicit def optional[T:ColumnType]:OptionalAtomicDbType[T] = OptionalAtomicDbType[T]

  def optional[T:ColumnType](name:String):OptionalNamedAtomicDbType[T] = OptionalAtomicDbType[T].apply(name)

  
}


