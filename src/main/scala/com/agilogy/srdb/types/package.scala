package com.agilogy.srdb

import java.sql.{ResultSet, PreparedStatement}

package object types extends ColumnTypeInstances with DbTypeCombinators{

  implicit def notNull[T:ColumnType]:NotNullAtomicDbType[T] = NotNullAtomicDbType[T]

  implicit def optional[T:ColumnType]:OptionalAtomicDbType[T] = OptionalAtomicDbType[T]

  def notNull[T:ColumnType](name:String):NotNullAtomicNamedDbReader[T] = NotNullAtomicNamedDbReader[T](name)

  def optional[T:ColumnType](name:String):OptionalAtomicNamedReader[T] = OptionalAtomicNamedReader[T](name)

  def set[T:DbWriter](ps:PreparedStatement,value:T):Unit = implicitly[DbWriter[T]].set(ps,value)
  def get[T:DbReader](rs:ResultSet):T = implicitly[DbReader[T]].get(rs)

  type ArgumentsSetter = PreparedStatement => Unit
  type ResultSetReader[T] = ResultSet => T

  implicit class DbWriterArgumentsSetter[T:DbWriter](value:T) extends ArgumentsSetter {
    override def apply(ps: PreparedStatement): Unit = set(ps,value)
  }

  implicit class DbReaderResultSetReader[T](dbReader:DbReader[T]) extends ResultSetReader[T] {
    override def apply(rs: ResultSet): T = get(rs)(dbReader)
  }

}
