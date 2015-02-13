package com.agilogy.srdb

import java.sql.{ResultSet, PreparedStatement}

package object types extends AtomicDbTypeImplicits with DbTypeImplicits with DbReaderImplicits {

  class NullColumnReadException extends RuntimeException

  def set[T:DbWriter](ps:PreparedStatement,value:T):Unit = implicitly[DbWriter[T]].set(ps,1,value)
  def set[T:DbWriter](ps:PreparedStatement, pos:Int, value:T):Unit = implicitly[DbWriter[T]].set(ps,pos,value)
  def get[T:DbReader](rs:ResultSet):T = implicitly[DbReader[T]].get(rs)
  def get[T:DbReader](rs:ResultSet, pos:Int):T = implicitly[DbReader[T]].get(rs,pos)
  def get[T:DbReader](rs:ResultSet, name:String):T = implicitly[DbReader[T]].get(rs,name)

  type ArgumentsSetter = PreparedStatement => Unit
  type ResultSetReader[T] = ResultSet => T
  
  implicit class DbWriterArgumentsSetter[T:DbWriter](value:T) extends ArgumentsSetter {
    override def apply(ps: PreparedStatement): Unit = set(ps,value)
  }

  implicit class DbReaderResultSetReader[T](dbReader:DbReader[T]) extends (ResultSet => T) {
    override def apply(rs: ResultSet): T = get(rs)(dbReader)
  }
  
  def dbType[T:DbType]:DbType[T] = implicitly[DbType[T]]

  def reader[T:DbReader]: ResultSet => T = rs => implicitly[DbReader[T]].get(rs,1)

  def reader[T:DbReader](name:String):NamedDbReader[T] = NamedDbReader[T](implicitly[DbReader[T]],name)
}
