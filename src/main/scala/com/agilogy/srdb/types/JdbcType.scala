package com.agilogy.srdb.types

import java.sql.Types

/**
 * A type according to the JDBC specification
 *
 * This class is only used because JDBC, until 3.0, uses `Int`s  stead of typed instances
 *
 * @param code The JDBC `Int` code of the JDBC type
 * @group API
 */
sealed abstract class JdbcType(val code: Int)

/** @group API */
object JdbcType {
  case object TinyInt extends JdbcType(Types.TINYINT)
  case object SmallInt extends JdbcType(Types.SMALLINT)
  case object Integer extends JdbcType(Types.INTEGER)
  case object BigInt extends JdbcType(Types.BIGINT)

  case object Boolean extends JdbcType(Types.BOOLEAN)

  case object Varchar extends JdbcType(Types.VARCHAR)
  case object Char extends JdbcType(Types.CHAR)
  case object LongVarchar extends JdbcType(Types.LONGNVARCHAR)

  case object Date extends JdbcType(Types.DATE)
  case object Timestamp extends JdbcType(Types.TIMESTAMP)

  case object Numeric extends JdbcType(Types.NUMERIC)
  case object Decimal extends JdbcType(Types.DECIMAL)
  case object Float extends JdbcType(Types.FLOAT)
  case object Double extends JdbcType(Types.DOUBLE)

  case object Array extends JdbcType(Types.ARRAY)

}

