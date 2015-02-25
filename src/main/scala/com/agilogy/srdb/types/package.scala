package com.agilogy.srdb

import java.sql.{ResultSet, PreparedStatement}

package object types extends ColumnTypeInstances with DbReaderImplicits with DbTypeCombinators{

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

  implicit class ColumnTypeResultSetReader[T](columnType:ColumnType[T]) extends ResultSetReader[T] {
    override def apply(rs: ResultSet): T = get(rs)(atomicDbType(columnType))
  }
  
  implicit def atomicDbType[T:ColumnType]:PositionalDbType[T] = new PositionalDbType[T] {

    override def get(rs: ResultSet, pos: Int): T = implicitly[ColumnType[T]].get(rs,pos)

    override def set(ps: PreparedStatement, pos:Int, value: T): Unit = implicitly[ColumnType[T]].set(ps,pos,value)

    override val length: Int = 1
  }

}
