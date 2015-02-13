package com.agilogy.srdb

import java.sql.{ResultSet, Types, PreparedStatement}


package object types {

  class NullColumnReadException extends RuntimeException
  
  def set[T:DbType](ps:PreparedStatement, pos:Int, value:T):Unit = implicitly[DbType[T]].set(ps,pos,value)
  def get[T:PositionalDbType](rs:ResultSet, pos:Int):T = implicitly[PositionalDbType[T]].get(rs,pos)
  def get[T:NamedDbType](rs:ResultSet, name:String):T = implicitly[NamedDbType[T]].get(rs,name)

  type ArgumentsSetter = PreparedStatement => Unit
  type Reader[T] = ResultSet => T
  
  implicit class ArgumentsSetterFromValue[T:DbType](value:T) extends ArgumentsSetter {
    override def apply(ps: PreparedStatement): Unit = set(ps,1,value)
  }

  def reader[T:PositionalDbType]: ResultSet => T = rs => implicitly[PositionalDbType[T]].get(rs,1)
  
  implicit class NamedDbTypeReader[T](dbType:NamedDbType[T]) extends (ResultSet => T) {
    override def apply(rs: ResultSet): T = dbType.get(rs)
  }

  def reader[T:DbType](name:String):NamedDbType[T] = NamedDbType(name)

}
