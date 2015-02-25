package com.agilogy.srdb

import java.sql.{ResultSet, PreparedStatement}

package object types extends ColumnTypeInstances with DbReaderImplicits with DbTypeImplicits{

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
