package com.agilogy.srdb.types

case class ColumnWriteException(pos: Int, causedBy: Throwable) extends RuntimeException(s"Exception writing to column in position $pos", causedBy)
