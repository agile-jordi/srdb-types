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
  
  implicit class DbTypeWithNameAccessReader[T](dbType:DbTypeWithNameAccess[T]) extends (ResultSet => T) {
    override def apply(rs: ResultSet): T = dbType.get(rs)
  }
  
  def dbType[T:PositionalDbType]:PositionalDbType[T] = implicitly[PositionalDbType[T]]

  def reader[T:PositionalDbType]: ResultSet => T = rs => implicitly[PositionalDbType[T]].get(rs,1)

  def reader[T:AtomicDbType](name:String):NamedDbType[T] = NamedDbType[T](implicitly[AtomicDbType[T]],name)

  def combine[T1,T2](dbTypes:(DbTypeWithNameAccess[T1],DbTypeWithNameAccess[T2])):DbTypeWithNameAccess[(T1,T2)] = new DbTypeWithNameAccess[(T1,T2)] {
    override def get(rs: ResultSet, name: String): (T1, T2) = (dbTypes._1.get(rs,name),dbTypes._2.get(rs,name))

    private val (t1,t2) = dbTypes

    override def set(ps: PreparedStatement, pos: Int, value: (T1, T2)): Unit = {
      t1.set(ps, pos, value._1)
      t2.set(ps, pos + t1.length, value._2)
    }

    override val length: Int = t1.length + t2.length

    override def get(rs: ResultSet): (T1, T2) = (dbTypes._1.get(rs),dbTypes._2.get(rs))
  }

}
