package com.agilogy.srdb.types

import java.sql.PreparedStatement

trait DbWriter[T] {

  self =>

  val length: Int

  def set(ps: PreparedStatement, value: T): Unit = set(ps,1,value)

  def set(ps: PreparedStatement, pos:Int, value: T): Unit

}

