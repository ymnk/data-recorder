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
import com.google.appengine.api.datastore.Text

@PersistenceCapable{val identityType = IdentityType.APPLICATION,
                    val detachable="true"}
class Data {

  @PrimaryKey
  @Persistent{val valueStrategy = IdGeneratorStrategy.IDENTITY}
  @Extension{val vendorName="datanucleus", 
             val key="gae.encoded-pk", 
             val value="true"}
  var id : String = _ 

  @Persistent
  var title : String = ""

  @Persistent
  var description : String = ""

  @Persistent
  var image : Text = new Text("")

  @Persistent
  var cumulative : java.lang.Boolean = java.lang.Boolean.FALSE

  @Persistent
  var user : User = _

  def save()={
    val data = this
    Model.withPM{ pm =>
      if(data.id == null){
        data.user.data.add(data)
        pm.makePersistent(data.user)
      }
      else{
        pm.makePersistent(data)
      }
    }
  }

  def delete()={
    val data = this 
    User.userVar(None)
    Model.withPM{ _.deletePersistent(data)}
  }
}

object Data{
  import Model._
  def findAll() = Model.withPM{ from(_, classOf[Data]).resultList }
  def create() = new Data
}
