/*
 * Copyright 2009 ymnk, JCraft,Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package com.jcraft.lift.model
import javax.jdo.annotations._

@PersistenceCapable{val identityType = IdentityType.APPLICATION,
                    val detachable="true"}
class DataEntry {

  @PrimaryKey
  @Persistent{val valueStrategy = IdGeneratorStrategy.IDENTITY}
  var id : java.lang.Long = _

  @Persistent
  var data : String = _

  @Persistent
  var date : java.lang.Integer = _

  @Persistent
  var dataEntry : java.lang.String = ""

  def delete()={
    val de = this 
    Model.withPM{ _.deletePersistent(de)}
  }

  // TODO DataEntry must be unique for date field.
  def save()={
    val de = this 
    Model.withPM{_.makePersistent(de)}
  }
}

object DataEntry{
  import _root_.java.util.Date
  private val formatter = new java.text.SimpleDateFormat("yyyyMMdd")
  def today = formatter.format(new Date).toInt
  def create() = new DataEntry match{
    case de =>
      de.date = today
      de
  }
}
