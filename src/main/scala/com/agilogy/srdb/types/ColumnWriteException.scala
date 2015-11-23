package com.agilogy.srdb.types

case class ColumnWriteException(pos: Int, causedBy: Throwable) extends RuntimeException {
  override def getMessage: String = s"Exception writing to column in position $pos"
}
